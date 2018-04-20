open Util
open Pkg
open Status
open Configuration
open Repository_search
open Repository
open Installation
open Installed_package

(* type dependency_graph = (pkg, installation_reason * (repository * pkg) list) Hashtbl.t *)

(* Worong_pkg (repository, sticky) *)
type specific_igraph_node =
    Present_pkg of bool |
    Wrong_pkg of (repository * bool) |
    Missing_pkg of repository

(* Installation reason, [(constraint, source)], infered version, hook *)
type igraph_node =
    installation_reason *
    (package_constraint * string option) list * version *
    specific_igraph_node

(* Name, node, dependencies, dependents (reverse dependencies) *)
type igraph = (string, igraph_node * string list * string list) Hashtbl.t

type ncrrl = (
    string *
    (package_constraint * string option) list *
    installation_reason *
    bool
) list

let rec add_node_to_igraph (cfg : configuration) (status : status)
    (ig : igraph) (name, constraints, reason, reinstall) =

    let add_dependencies (sp : static_pkg) =
        add_nodes_to_igraph cfg status ig
            (List.map
                (fun (n,cs) ->
                    let cs =
                        List.map (fun c -> (c, Some sp.sn)) cs
                    in
                    (n, cs, Auto, false))
                sp.sdeps)
    in
    let add_missing_package (cs : annotated_package_constraint list) =
        match
            find_and_select_package_in_all_repos
                name
                (List.map (fun (c, _) -> c) cs)
                cfg.a
        with
            | None -> raise (Gp_exception ("Package \"" ^ name ^
                "\" not found or an impossible situation was requested"))
            | Some (r, sp) ->
                Hashtbl.replace ig name
                    ((reason, cs, sp.sv, Missing_pkg r),
                    List.map (fun (n,_) -> n) sp.sdeps, []);
                add_dependencies sp
    in
    let add_installed_package
        (s_pkg : pkg)
        (s_status : installation_status)
        (s_reason : installation_reason)
        (cs : annotated_package_constraint list)
        (reinstall : bool) =
        let s_sp =
            match static_of_dynamic_pkg s_pkg with
                | None ->
                    raise
                        (Gp_exception "Invalid package in status")
                | Some sp -> sp
        in
        let reason = max_reason [s_reason; reason]
        in
        let rc = s_reason <> reason
        in
        let (sp, node) =
            match
                find_and_select_package_in_all_repos
                    name
                    (List.map (fun (c, _) -> c) cs)
                    cfg.a
            with
                | Some (repo, sp) when
                    compare_version sp.sv s_sp.sv <> 0 || reinstall ->

                    (sp, (reason, cs, sp.sv, Wrong_pkg (repo, reinstall)))

                | Some _
                | None ->
                    match
                        cs_satisfied
                            s_sp.sv
                            (List.map (fun (c,_) -> c) cs)
                    with
                        | false ->
                            raise
                                (Gp_exception ("Package \"" ^ name ^
                                "\" was not found in any repository and the " ^
                                "installed version is not sufficient, or " ^
                                "an impossible situation was requested"))
                        | true ->
                            (s_sp, (reason, cs, s_sp.sv, Present_pkg rc))
        in
        Hashtbl.replace
            ig
            name
            (node, List.map (fun (n, _) -> n) sp.sdeps, []);
        add_dependencies sp
    in
    let remove_from_igraph (ig : igraph) (name : string) =
        match Hashtbl.find_opt ig name with
            | None ->
                raise
                    (Gp_exception ("Package \"" ^ name ^
                    "\" shall be removed however it is not in the graph"))
            | Some (node, deps, dets) ->
                Hashtbl.remove ig name;
                (* Remove constraints and reverse edges from dependencies *)
                List.iter
                    (fun n ->
                        match Hashtbl.find_opt ig n with
                            | None -> ()
                            | Some ((reason, cs, v, spec), deps, dets) ->
                                let cs =
                                    List.filter
                                        (fun (c, so) ->
                                            match so with
                                                | Some s when s = name -> false
                                                | _ -> true)
                                    cs
                                in
                                Hashtbl.replace
                                    ig
                                    n
                                    ((reason, cs, v, spec), deps, dets))
                    deps
    in
    let update_constraints_reason
        ((node : igraph_node), (deps : string list), (dets : string list))
        (reason : installation_reason)
        (cs : annotated_package_constraint list) =

        let (cr, _, v, spec) =
            node
        in
        let node =
            match spec with
                | Present_pkg rc ->
                    let rc = rc || reason <> cr
                    in
                    (reason, cs, v, Present_pkg rc)
                | Wrong_pkg x -> (reason, cs, v, Wrong_pkg x)
                | Missing_pkg x -> (reason, cs, v, Missing_pkg x)
        in
        Hashtbl.replace
            ig
            name
            (node, deps, dets)
    in
    match Hashtbl.find_opt ig name with
        | Some (node, deps, dets) ->
            let (ir, cs, v, spec) =
                node
            in
            (* Determine if the selected package must be replaced *)
            let nr = max_reason [ir; reason]
            in
            let cs =
                merge_annotated_constraints cs constraints
            in
            let reinstall =
                match spec with
                    | Present_pkg _ -> reinstall
                    | Wrong_pkg (_, s) -> s || reinstall
                    | Missing_pkg _ -> reinstall
            in
            (match
                not (cs_satisfied
                    v
                    (List.map (fun (c,_) -> c) cs)) ||
                reinstall
            with
                | false ->
                    update_constraints_reason
                        (node, deps, dets)
                        nr
                        cs
                | true ->
                    (* Remove the node *)
                    remove_from_igraph ig name;
                    (* Add the node with the new parameters *)
                    add_node_to_igraph cfg status ig (name, cs, nr, reinstall))
        | None ->
            (match select_status_tuple_by_name status name with
                | None -> add_missing_package constraints
                | Some (sp, sr, ss) ->
                    add_installed_package sp ss sr constraints reinstall)
        (* | _ -> raise (Gp_exception "Not implemented yet") *)

and add_nodes_to_igraph
    (cfg : configuration)
    (status : status)
    (ig : igraph)
    (ncrrl : ncrrl) =

    List.iter
        (add_node_to_igraph cfg status ig)
        ncrrl

let add_reverse_edges_to_igraph (ig : igraph) =
    let process_node (name, (_, deps, _)) =
        List.iter
            (fun dn ->
                match Hashtbl.find_opt ig dn with
                    | None ->
                        raise (Gp_exception "Package not in graph")
                    | Some (node, deps, dets) ->
                        Hashtbl.replace ig dn (node, deps, name::dets))
            deps
    in
    List.iter
        process_node
        (hashtbl_kv_pairs ig)

let build_igraph (cfg : configuration) (status : status) (ncrrl : ncrrl) =
    let ig = Hashtbl.create ~random:true 100
    in
    try
        let existing_ncrrl =
            List.map
                (fun (pkg, reason, pstate) ->
                    let name =
                        match pkg.n with
                            | None ->
                                raise
                                    (Gp_exception
                                    "Status tuple with package with no name")
                            | Some n -> n
                    in
                    (name, [], reason, false))
                (select_all_status_tuples status)
        in
        add_nodes_to_igraph cfg status ig existing_ncrrl;
        add_nodes_to_igraph cfg status ig ncrrl;
        add_reverse_edges_to_igraph ig;
        Some ig
    with
        Gp_exception msg -> print_endline ("Depres: " ^ msg); None

(* let manual_pkgs_of_igraph (ig : igraph) =
    Hashtbl.fold
        (fun name (node, edges) mpkgs ->
            match node with
                | Present_pkg (Manual, _, _) -> name :: mpkgs
                | Wrong_pkg (Manual, _, _) -> name :: mpkgs
                | Missing_pkg (Manual, _, _) -> name :: mpkgs
                | _ -> mpkgs)
            ig
            [] *)

let install_from_igraph
    (cfg : configuration) (ig : igraph) (status : status option) =
    match status with None -> None | Some status ->
    let visited_set = Hashtbl.create 100
    in
    let rec process_child status name =
        match status with None -> None | Some status ->
        if not (Hashtbl.mem visited_set name)
        then visit_node status name
        else Some status
    and visit_node status name =
        match Hashtbl.find_opt ig name with
            | None -> print_endline "Depres: Package not in graph"; None
            | Some (node, deps, dets) ->
                Hashtbl.add visited_set name ();
                let status =
                    List.fold_left
                        process_child
                        (Some status)
                        deps;
                in
                match node with
                    | (reason, constraints, version, Missing_pkg repo) ->
                        elementary_install_package
                            cfg
                            name
                            version
                            reason
                            repo
                            status
                    | (reason, constraints, version, Wrong_pkg (repo, _)) ->
                        elementary_change_package
                            cfg
                            name
                            version
                            reason
                            repo
                            status
                    | (reason, constraints, version, Present_pkg rc) ->
                        (match rc with
                            | false -> status
                            | true ->
                                (match reason with
                                    | Manual -> mark_package_manual name
                                    | Auto -> mark_package_auto name)
                                    status)
    in
    List.fold_left
        process_child
        (Some status)
        (hashtbl_keys ig)

let configure_from_igraph
    (cfg : configuration) (ig : igraph) (status : status option) =
    match status with None -> None | Some status ->

    let visited_set =
        Hashtbl.create ~random:true 100
    in
    let reset_dependents
        (ig : igraph)
        (status : status option) =

        match status with None -> None | Some status ->
        let visited_set =
            Hashtbl.create ~random:true 100
        in
        let rec infect_parent (status : status option) (name : string) =
            match status with None -> None | Some status ->
            match Hashtbl.find_opt visited_set name with
                | Some true -> Some status
                | Some false
                | None ->
            match select_status_tuple_by_name status name with
                | None ->
                    print_endline "Depres: Package not in status";
                    None
                | Some (pkg, reason, Configured) ->
                    Hashtbl.replace visited_set name true;
                    let status =
                        update_status_tuple status (pkg, reason, Configuring)
                    in
                    (match Hashtbl.find_opt ig name with
                        | None ->
                            print_endline "Depres: Package not in graph";
                            None
                        | Some (node, deps, dets) ->
                    List.fold_left
                        infect_parent
                        (Some status)
                        dets)
                | Some (_, _, _) -> Some status
            
        in
        let rec process_child
            (status : status option)
            ((name : string), (_, _, (dets : string list))) =

            match status with None -> None | Some status ->
            match Hashtbl.mem visited_set name with
                | true -> Some status
                | false -> visit_node name dets status

        and visit_node (name : string) (dets : string list) (status : status) =
            Hashtbl.add visited_set name false;
            match select_status_tuple_by_name status name with
                | None ->
                    print_endline "Depres: Package not in status";
                    None
                | Some (_, _, pstate) ->
            match pstate with
                | Configured
                | Changing_unconf
                | Changing
                | Installing
                | Removing_unconf
                | Removing -> Some status
                | Installed
                | Configuring ->
                    List.fold_left
                        infect_parent
                        (Some status)
                        dets
        in
        List.fold_left
            process_child
            (Some status)
            (hashtbl_kv_pairs ig)
    in
    let rec process_child (status : status option) (name : string) =
        match status with None -> None | Some status ->
        match Hashtbl.mem visited_set name with
            | true -> Some status
            | false ->
                visit_node name status
    and visit_node (name : string) (status : status) =
        Hashtbl.add visited_set name ();
        match select_status_tuple_by_name status name with
            | None ->
                print_endline ("Depres: Package \"" ^ name ^
                "\" not in status");
                None
            | Some (_, _, pstate) ->
                match pstate with
                    | Installing
                    | Configured
                    | Changing
                    | Changing_unconf
                    | Removing
                    | Removing_unconf -> Some status
                    | Installed
                    | Configuring ->
                        match Hashtbl.find_opt ig name with
                            | None ->
                                print_endline "Depres: Package not in graph";
                                None
                            | Some (_, deps, dets) ->
                                let status =
                                    List.fold_left
                                        process_child
                                        (Some status)
                                        deps
                                in
                                elementary_configure_package name status
    in
    let status =
        reset_dependents ig (Some status)
    in
    List.fold_left
        process_child
        status
        (hashtbl_keys ig)

let install_configure_from_igraph
    (cfg : configuration) (status : status) (ig : igraph) =

    install_from_igraph cfg ig (Some status)
    |>
    match !runtime_system with
        | Native_runtime -> configure_from_igraph cfg ig
        | Directory_runtime _ -> fun x -> x

let remove_from_igraph (status : status) (ig : igraph) (names : string list) =
    let visited_set =
        Hashtbl.create ~random:true 100
    in
    let rec process_parent (status : status option) (name : string) =
        match status with None -> None | Some status ->
        match Hashtbl.mem visited_set name with
            | true -> Some status
            | false -> visit_child name status

    and visit_child (name : string) (status : status) =
        Hashtbl.add visited_set name ();
        match Hashtbl.find_opt ig name with
            | None ->
                print_endline "Depres: Package not in graph";
                None
            | Some (_, _, dets) ->
        let status =
            List.fold_left
                process_parent
                (Some status)
                dets
        in
        match status with
            | None -> None
            | Some status ->
        match select_status_tuple_by_name status name with
            | None ->
                print_endline
                    ("Package \"" ^ name ^
                    "\" is not installed hence not removing it.");
                Some status
            | Some _ ->
        elementary_remove_package name (Some status)
    in
    List.fold_left
        process_parent
        (Some status)
        names

let print_igraph names =
    print_target ();
    match read_configuration () with None -> false | Some cfg ->
    match read_status () with None -> false | Some status ->
    match
        List.fold_left
            (fun a n ->
                match a with None -> None | Some a ->
                match pkg_name_constraints_of_string n with
                    | None ->
                        print_endline
                            ("Invalid package description \"" ^ n ^ "\"");
                            None
                    | Some (n, cs) ->
                        let cs =
                            List.map (fun c -> (c, None)) cs
                        in
                        Some ((n, cs, Manual, false)::a))
            (Some [])
            names
    with
        | None -> false
        | Some ncrrl ->
    match build_igraph cfg status ncrrl with
        | None -> false
        | Some ig ->

    let string_of_cs cs =
        ui_string_of_annotated_constraints cs
    in
    let pf =
        Perf_hash.create_empty ()
    in
    print_endline "digraph Dependencies {";
    Hashtbl.fold
        (fun name (node, deps, dets) pf ->
            let (pf, nc) =
                Perf_hash.map pf name
            in
            let nc =
                string_of_int nc
            in
            print_endline (nc ^ " [label=\"" ^
            (match node with
                | (ir, cs, v, Present_pkg rc) -> "Present_pkg (" ^ name ^ ", " ^
                    string_of_installation_reason ir ^ ", " ^
                    string_of_cs cs ^ ", " ^
                    string_of_version v ^ ", " ^ string_of_bool rc ^ ")"
                | (ir, cs, v, Wrong_pkg (r, s)) ->
                    "Wrong_pkg (" ^ name ^ ", " ^
                    string_of_installation_reason ir ^ ", " ^
                    string_of_cs cs ^ ", " ^
                    string_of_version v ^ ", (_, " ^
                    string_of_bool s ^ "))"
                | (ir, cs, v, Missing_pkg r) -> "Missing_pkg (" ^ name ^ ", " ^
                    string_of_installation_reason ir ^ ", " ^
                    string_of_cs cs ^ ", " ^
                    string_of_version v ^ ", _)")
            ^ "\"];");
            let pf =
                List.fold_left
                    (fun pf edge ->
                        let (pf, ec) =
                            Perf_hash.map pf edge
                        in
                        print_endline (nc ^ " -> " ^ string_of_int ec ^ ";");
                        pf)
                    pf
                    deps
            in
            let pf =
                List.fold_left
                    (fun pf redge ->
                        let (pf, ec) =
                            Perf_hash.map pf redge
                        in
                        print_endline (nc ^ " -> " ^ string_of_int ec ^
                            "[color=red, style=dotted];");
                        pf)
                    pf
                    dets
            in
            pf)
        ig
        pf
    |> ignore;
    print_endline "}";
    true

(* let get_dependent_package_names tuple_predicate status name =
    let dep_set =
        Hashtbl.create 100
    in
    let registered_set =
        List.map
            (fun (p,ir,ps) ->
                match static_of_dynamic_pkg p with
                    | None -> print_endline "Depres: Invalid package";
                        raise Gp_exception
                    | Some spkg -> (spkg, p, ir, ps))
            (select_all_status_tuples status)
    in
    let bridge change name =
        List.fold_left
            (fun change (spkg, pkg, ir, ps) ->
                if
                    List.exists (fun n -> compare_names n name = 0) spkg.srdeps
                then
                    if Hashtbl.mem dep_set spkg.sn
                    then change
                    else (Hashtbl.add dep_set spkg.sn (pkg, ir, ps); true)
                else change)
            change
            registered_set
    in
    let change =
        bridge false name
    in
    let rec transitive_bridge_step () =
        let current_dep_set =
            hashtbl_keys dep_set
        in
        let change =
            List.fold_left
                bridge
                false
                current_dep_set
        in
        if change
        then transitive_bridge_step ()
        else ()
    in
    if change
    then transitive_bridge_step ()
    else ();
    Hashtbl.fold
        (fun n t l -> if tuple_predicate t then n::l else l)
        dep_set
        []

let print_reverse_dependencies name =
    print_target ();
    match read_status () with None -> raise Gp_exception | Some status ->
    let installed_deps =
        get_dependent_package_names
            (fun (_,_,s) -> is_installed_of_state s)
            status
            name
    in
    let installed_deps =
        List.sort
            compare_names
            installed_deps
    in
    print_endline
        ("The following installed packages depend on a package with name \"" ^
        name ^ "\"");
    List.iter
        print_endline
        installed_deps *)
