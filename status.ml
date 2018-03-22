open Xml
open Util
open Pkg

type installation_status = Installation | Installed | Removal | Upgrade
type installation_reason = Auto | Manual
type status_tupel = pkg * installation_reason * installation_status
type status = status_tupel list

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
    | "removal" -> Some Removal
    | "upgrade" -> Some Upgrade
    | _ -> None

let string_of_installation_status = function
    | Installation -> "installation"
    | Installed -> "installed"
    | Removal -> "removal"
    | Upgrade -> "upgrade"

type dynamic_status_tupel =
    pkg option *
    installation_reason option *
    installation_status option

let dynamic_to_static_status_tupel = function
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
    let process_tupel_element v (pkg, reason, status) = function
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
            print_endline ("Status: Invalid text in tupel: \"" ^ t ^ "\"");
            None
    in
    let process_toplevel_element v = function
        | Element ("tupel",_,cs) ->
            (let dst = List.fold_left
                (fun dst e -> match dst with None -> None | Some dst ->
                    process_tupel_element v dst e)
                (Some (None, None, None))
                cs
            in
            match dst with None -> None | Some dst ->
            let st = dynamic_to_static_status_tupel dst
            in
            match st with
                | None -> print_endline "Status: Information missing in tupel"; None
                | Some st -> Some st)
        | Element (v,_,_) | PCData v ->
            print_endline ("Status: Invalid toplevel element: \"" ^ v ^ "\"");
            None
    in
    let process_toplevel_elements v elems =
        List.fold_left
            (fun a e -> match a with None -> None | Some a ->
                match process_toplevel_element v e with None -> None | Some t ->
                    Some (t::a))
            (Some [])
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
    let xml_of_tupel (pkg, r, s) =
        match xml_of_pkg pkg with
            | None -> None
            | Some xpkg -> Some (Element ("tupel", [], [
                    xpkg;

                    Element ("reason", [],
                        [PCData (string_of_installation_reason r)]);

                    Element ("status", [],
                        [PCData (string_of_installation_status s)])
                ]))
    in
    let xtupels =
        List.fold_left
            (fun a t -> match a with None -> None | Some a ->
                match xml_of_tupel t with None -> None | Some x -> Some (x::a))
            (Some [])
            (List.rev s)
    in
    match xtupels with
        | None -> (print_endline "Status: Invalid package"; false)
        | Some xtupels ->
            let x = Element ("status", [("file_version", "1.0")], xtupels)
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

let compare_tupels (p1,_,_) (p2,_,_) = match (p1.n, p2.n) with
        | (Some n1, Some n2) -> compare_names n1 n2
        | _ -> failwith "Status: Uncomparable tupel"

let rec select_status_tupel_by_name status name =
    match status with
        | [] -> None
        | dt :: sts ->
            if  compare_tupels dt (
                    {empty_pkg with n = Some name},
                    (),
                    ()) = 0
            then Some dt
            else select_status_tupel_by_name sts name

let unique_insert_status_tupel status tupel =
    sorted_unique_insert compare_tupels status tupel

let update_status_tupel status tupel =
    List.map (fun t -> if (compare_tupels t tupel) = 0 then tupel else t) status

let delete_status_tupel status tupel =
    List.fold_left
        (fun a t -> if (compare_tupels t tupel) = 0 then a else t::a)
        []
        (List.rev status)