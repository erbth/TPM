open Xml
open Util
open Repository

type configuration = {
    repos:repository list;
    a:arch
}

type dynamic_configuration = {
    repos:repository list;
    a:arch option
}

(* In the dynamic configuration, the repository list is in reverse order. This
 * allows to add repositories easily. *)
let empty_dynamic_configuration = {repos = []; a = None}
let dynamic_to_static_configuration : dynamic_configuration -> configuration option = function
    | {repos = repos; a = Some a} -> Some {repos = List.rev repos; a = a}
    | _ -> None

let read_configuration () =
    let process_element dcfg = function
        | Element ("arch",_,cs) -> (match cs with
            | [PCData a] -> Some {dcfg with a = arch_of_string a}
            | _ -> print_endline ("Config file: Invalid architecture"); None)
        | Element ("repo",attrs,cs) -> (match List.assoc_opt "type" attrs with
            | Some "dir" -> (match cs with
                | [PCData p] ->
                    Some {dcfg with repos = (DirectoryRepository p)::dcfg.repos}
                | _ -> print_endline "Config file: Invalid repository"; None)
            | Some t ->
                print_endline ("Config file: Invalid repository type \"" ^
                    t ^ "\""); None
            | _ -> print_endline "Config file: Repository type missing"; None)
        | Element (t,_,_) -> print_endline ("Config file: Invalid element: " ^ t); None
        | _ -> print_endline "Config file: Invalid element"; None
    in
    let process_elements elems =
        let dcfg_opt =
        List.fold_left
            (fun a e -> match a with None -> None | Some a -> process_element a e)
            (Some empty_dynamic_configuration)
            elems
        in
        match dcfg_opt with
            | None -> None
            | Some cfg -> dynamic_to_static_configuration cfg
    in
    let cfile = form_target_path Tpm_config.config_file_path
    in
    try
        match parse_file cfile with
            | Element ("tpm",attrs,cs) ->
                (match List.assoc_opt "file_version" attrs with
                    | Some "1.0" -> process_elements cs
                    | Some v -> print_endline ("Config file: Invalid version: " ^ v); None
                    | _ -> print_endline "Config file: Version missing"; None)
            | _ -> print_endline "Config file: Invalid root element"; None
    with _ -> print_endline ("Config file: Could not read \"" ^ cfile ^ "\""); None