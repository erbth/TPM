open Util
open Pkg
open Status
open Configuration
open Repository_search
open Repository

type dependency_graph = (pkg, installation_reason * (repository * pkg) list) Hashtbl.t

let rec add_node_to_dependency_graph (cfg: configuration) g (repo, pkg, reason) =
    match (Hashtbl.mem g pkg) with
        | true -> ()
        | false -> (
            let rdep_repo_pkg_list =
                List.map
                    (fun n -> match find_and_select_package_in_all_repos n with
                        | Some (r,p) -> (r,p)
                        | None -> failwith ("Depres: Package \"" ^ n ^
                            "\" not found for architecture \"" ^
                            (string_of_arch cfg.a) ^ "\""))
                    pkg.rdeps
            in
            Hashtbl.add g pkg (reason, rdep_repo_pkg_list);
            List.map (fun (r,p) -> (r, p, Auto)) rdep_repo_pkg_list
            |> add_nodes_to_dependency_graph cfg g
        )

and add_nodes_to_dependency_graph cfg g repo_pkg_reason_list =
    List.iter (add_node_to_dependency_graph cfg g) repo_pkg_reason_list

let create_dependency_graph () = Hashtbl.create ~random:true 1000

let print_dependency_graph names =
    print_target ();
    match read_configuration () with None -> raise Gp_exception | Some cfg ->
    match read_status () with None -> raise Gp_exception | Some status ->
    let g =
        create_dependency_graph ()
    in
    let rprs =
        List.map
            (fun n -> match find_and_select_package_in_all_repos n with
                | None -> print_endline ("Depres: Package \"" ^ n ^
                    "\" not found for architecture \"" ^ (string_of_arch cfg.a) ^
                    "\""); raise Gp_exception
                | Some (r, p) -> (r, p, Manual))
            names
    in
    (try
        add_nodes_to_dependency_graph cfg g rprs
    with
        Failure msg -> print_endline msg;
            raise Gp_exception);
    print_endline "digraph Dependencies {";
    Hashtbl.iter
        (fun pkg (r, rp_deps) -> print_endline
            (string_of_pkg pkg ^ " [label=\"" ^ string_of_pkg pkg ^ " : " ^
            (match select_status_tuple_by_pkg status pkg with
                | None -> "---"
                | Some (_,r,_) -> string_of_installation_reason r) ^
            "\"];"))
        g;
    Hashtbl.iter
        (fun pkg (r, rp_deps) -> if rp_deps <> []
            then List.iter
                    (fun (_,dp) -> print_endline (string_of_pkg pkg ^
                        " -> " ^ string_of_pkg dp ^ ";"))
                    rp_deps
            else ())
        g;
    print_endline "}";
    ()

let get_dependent_package_names tuple_predicate status name =
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
        installed_deps
