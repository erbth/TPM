open Xml
open Util
open Pkg

type installation_status =  Installation | Installed | Configuration |
                            Configured | Removal | Upgrade
type installation_reason = Auto | Manual
type status_tuple = pkg * installation_reason * installation_status
type status = (string, status_tuple) Hashtbl.t

let installation_reason_of_string = function
    | "auto" -> Some Auto
    | "manual" -> Some Manual
    | _ -> None

let string_of_installation_reason = function
    | Auto -> "auto"
    | Manual -> "manual"

let installation_status_of_string = function
    | "installation" -> Some Installation
    | "installed" -> Some Installed
    | "configuration" -> Some Configuration
    | "configured" -> Some Configured
    | "removal" -> Some Removal
    | "upgrade" -> Some Upgrade
    | _ -> None

let string_of_installation_status = function
    | Installation -> "installation"
    | Installed -> "installed"
    | Configuration -> "configuration"
    | Configured -> "configured"
    | Removal -> "removal"
    | Upgrade -> "upgrade"

let is_installed_of_state = function
    | Installed -> true
    | Configuration -> true
    | Configured -> true
    | _ -> false

let is_configured_of_state = function
    | Configured -> true
    | _ -> false

type dynamic_status_tuple =
    pkg option *
    installation_reason option *
    installation_status option

let dynamic_to_static_status_tuple = function
    | (Some p, Some r, Some s) -> Some (p, r, s)
    | _ -> None

let pot_create_status () =
    let sp = form_target_path Tpm_config.status_file_path
    in
    let status_file_directory =
        Tpm_config.status_file_path |> String.split_on_char '/' |>
        List.rev |> List.tl |> List.rev |> String.concat "/"
    in
    if Sys.file_exists sp then true else
    try
        mkdir_p_at_target status_file_directory 0o755;
        let pu = Unix.umask 0o022
        in
        let oc = open_out sp
        in
        let _ = Unix.umask pu
        in
        output_string oc (xml_to_string_with_desc
            (Element ("status", [("file_version", "1.0")], [])));
        close_out oc;
        true
    with
        | Unix.Unix_error (c,f,p) ->
            print_endline ("Status: Could not create initial \"" ^ sp ^ "\":");
            print_endline ("        " ^ Unix.error_message c ^ " (" ^ f ^ ", " ^ p ^ ")");
            false
        |_ ->
            print_endline ("Status: Could not create initial \"" ^ sp ^ "\"");
            false

let read_status () =
    let process_tuple_element v (pkg, reason, status) = function
        | Element ("reason",_,[PCData r]) ->
            (match installation_reason_of_string r with
                | None -> print_endline ("Status: Invalid reason \"" ^ r ^
                    "\""); None
                | Some r -> Some (pkg, Some r, status))
        | Element ("status",_,[PCData s]) ->
            (match installation_status_of_string s with
                | None -> print_endline ("Status: Invalid status \"" ^ s ^
                    "\""); None
                | Some s -> Some (pkg, reason, Some s))
        | Element (t,attr,cs) ->
            let attr = (match List.assoc_opt "file_version" attr with
                | Some _ -> attr
                | None -> ("file_version", v) :: attr)
            in
            (match pkg_of_xml (Element (t,attr,cs)) with
                | None -> print_endline "Status: Invalid package"; None
                | Some pkg -> Some (Some pkg, reason, status))
        | PCData t ->
            print_endline ("Status: Invalid text in tuple: \"" ^ t ^ "\"");
            None
    in
    let process_toplevel_element v = function
        | Element ("tuple",_,cs) ->
            (let dst = List.fold_left
                (fun dst e -> match dst with None -> None | Some dst ->
                    process_tuple_element v dst e)
                (Some (None, None, None))
                cs
            in
            match dst with None -> None | Some dst ->
            let st = dynamic_to_static_status_tuple dst
            in
            match st with
                | None -> print_endline "Status: Information missing in tuple"; None
                | Some st -> Some st)
        | Element (v,_,_) | PCData v ->
            print_endline ("Status: Invalid toplevel element: \"" ^ v ^ "\"");
            None
    in
    let process_toplevel_elements v elems =
        let s = Hashtbl.create ~random:true 1000
        in
        List.fold_left
            (fun s e -> match s with None -> None | Some s ->
                match process_toplevel_element v e with
                    | None -> None
                    | Some t ->
                        let (p,_,_) = t
                        in
                        Hashtbl.add s (unopt p.n) t; Some s)
            (Some s)
            (List.rev elems)
    in
    let sp = form_target_path Tpm_config.status_file_path
    in
    if not (pot_create_status ()) then None else
    let x =
        try
            Some (parse_file sp)
        with _ ->
            print_endline ("Status: Could not read status file \"" ^
                sp ^ "\""); None
    in
    match x with None -> None | Some x ->
    match x with
        | Element ("status", attrs, cs) ->(
            match List.assoc_opt "file_version" attrs with
                | Some v when v = "1.0" -> process_toplevel_elements v cs
                | Some v ->
                    print_endline ("Status: Invalid file version " ^ v);
                    None
                | _ -> print_endline "Status: File version missing"; None
        )
        | Element (e,_,_) | PCData e ->
            print_endline ("Status: Invalid element at top level: \"" ^ e
                ^ "\""); None

let write_status s =
    let xml_of_tuple (pkg, r, s) =
        match xml_of_pkg pkg with
            | None -> None
            | Some xpkg -> Some (Element ("tuple", [], [
                    xpkg;

                    Element ("reason", [],
                        [PCData (string_of_installation_reason r)]);

                    Element ("status", [],
                        [PCData (string_of_installation_status s)])
                ]))
    in
    let xtuples =
        Hashtbl.fold
            (fun n t a -> match a with None -> None | Some a ->
                match xml_of_tuple t with None -> None | Some x -> Some (x::a))
            s
            (Some [])
    in
    match xtuples with
        | None -> (print_endline "Status: Invalid package"; false)
        | Some xtuples ->
            let x = Element ("status", [("file_version", "1.0")], xtuples)
            in
            let sp = form_target_path Tpm_config.status_file_path
            in
            try
                let oc = open_out sp
                in
                xml_to_string_with_desc x |> output_string oc;
                close_out oc;
                true
            with
                | Sys_error msg -> print_endline (
                    "Status: Could not write to \"" ^ sp ^ "\": " ^ msg);
                    false
                | _ -> print_endline (
                    "Status: Could not write to \"" ^ sp ^ "\"");
                    false

let compare_status_tuples (p1,_,_) (p2,_,_) =
    compare_pkgs_by_name p1 p2

let select_all_status_tuples status =
    Hashtbl.fold
        (fun n t a -> t::a)
        status
        []

let rec select_status_tuple_by_name status name =
    Hashtbl.find_opt status name

let select_status_tuple_by_pkg status pkg =
    match pkg.n with
        | None -> None
        | Some name -> select_status_tuple_by_name status name

let select_status_tuple_by_predicate p status =
    Hashtbl.fold
        (fun _ t l -> if p t then t::l else l)
        status
        []

let unique_insert_status_tuple status (p,r,ps) =
    match p.n with
        | None -> status
        | Some name ->
            match Hashtbl.mem status name with
                | true -> status
                | false -> Hashtbl.add status name (p,r,ps); status

let update_status_tuple status (p,r,ps) =
    match p.n with
        | None -> status
        | Some name ->
            match Hashtbl.mem status name with
                | false -> status
                | true -> Hashtbl.replace status name (p,r,ps); status

let update_status_tuple_installation_reason status name reason =
    match select_status_tuple_by_name status name with
        | Some (p, r, s) -> update_status_tuple status (p, reason, s)
        | None -> status


let delete_status_tuple status (p,_,_) =
    match p.n with
        | None -> status
        | Some name -> Hashtbl.remove status name; status

let is_pkg_name_installed status name =
    match select_status_tuple_by_name status name with
        | Some (_,_,s) -> is_installed_of_state s
        | _ -> false

let is_pkg_installed status pkg =
    match pkg.n with
        | None -> false
        | Some n -> is_pkg_name_installed status n

let is_pkg_name_configured status name =
    match select_status_tuple_by_name status name with
        | Some (_,_,s) -> is_configured_of_state s
        | _ -> false

let is_pkg_configured status pkg =
    match pkg.n with
        | None -> false
        | Some n -> is_pkg_name_configured status n
