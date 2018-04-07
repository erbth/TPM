open Util
open Pkg
open Repository

let read_package path =
    if not (create_tmp_dir ()) then None
    else
    let cmd = !program_tar ^ " -xf " ^ path ^
        " -C " ^ Tpm_config.tmp_dir ^ " " ^ Tpm_config.desc_file_name
    in
    try
        if (Sys.command cmd) <> 0
        then (print_endline "Packed: Could not unpack the package (tar failed)";
            None)
        else
        (Xml.parse_file (Tpm_config.tmp_dir ^ "/" ^ Tpm_config.desc_file_name)
            |> pkg_of_xml)
    with
        | Sys_error msg -> print_endline
            ("Packed: Could not read the package: " ^ msg); None
        | _ -> print_endline ("Packed: Could not read the package"); None

let unpack_packed_form repo pkg =
    if not (create_tmp_dir ()) then false
    else
    match provide_transport_shape repo pkg with None -> false | Some path ->
    let unpack_package_cmd = !program_tar ^ " -xf " ^ path ^
        " -C " ^ Tpm_config.tmp_dir
    in
    try
        if (Sys.command unpack_package_cmd) <> 0
        then (print_endline "Packed: Could not unpack the package (tar failed)";
            false)
        else
            true
    with
        | Sys_error msg -> print_endline
            ("Packed: Failed to unpack the package: " ^ msg); false
        | _ -> print_endline "Packed: Failed to unpack the package"; false

let unpack_files_from_tmp pkg exclude_files =
    let str_exclude_files =
        List.map
            (fun fn ->
                let fn =
                    if String.length fn > 0
                    then
                        String.sub fn 1 (String.length fn - 1)
                    else fn
                in
                "--exclude='" ^ fn ^ "'")
            exclude_files
        |>
        String.concat " "
    in
    let install_files_cmd = !program_tar ^
        " -xpI " ^ !program_gzip ^
        " -f " ^ Tpm_config.tmp_dir ^ "/" ^ Tpm_config.destdir_name ^ ".tar.gz" ^
        " -C " ^ !target_system ^ " " ^ str_exclude_files
    in
    try
        if (Sys.command install_files_cmd) = 0 then true
        else
            (print_endline
                "Packed: Could not install the package's files (tar failed)";
            false)
    with
        | Sys_error msg -> print_endline
            ("Packed: Could not install the package's files: " ^ msg); false
        | _ -> print_endline ("Packed: Could not install the package's files"); false

let select_version_to_install :
    (repository * pkg) list -> (repository * pkg) option =
    List.fold_left
        (fun a rp -> match a with None -> Some rp | Some a ->
            if pkg_newer (snd rp) (snd a) then Some rp else Some a)
        None
