open Util
open Pkg
open Repository

let read_package path =
    if not (create_tmp_dir ()) then None
    else
    let cmd = Tpm_config.program_tar ^ " -xf " ^ path ^
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

let install_files repo pkg =
    if not (create_tmp_dir ()) then false
    else
    match provide_transport_shape repo pkg with None -> false | Some path ->
    let unpack_package_cmd = Tpm_config.program_tar ^ " -xf " ^ path ^
        " -C " ^ Tpm_config.tmp_dir ^ " " ^ Tpm_config.destdir_name ^ ".tar.gz"
    in
    let install_files_cmd = Tpm_config.program_tar ^
        " -xpI " ^ Tpm_config.program_gzip ^
        " -f " ^ Tpm_config.tmp_dir ^ "/" ^ Tpm_config.destdir_name ^ ".tar.gz" ^
        " -C " ^ !target_system
    in
    try
        if (Sys.command unpack_package_cmd) <> 0
        then (print_endline "Packed: Could not unpack the package (tar failed)";
            false)
        else
        if (Sys.command install_files_cmd) = 0 then true
        else
            (print_endline
                "Packed: Could not install the package's files (tar failed)";
            false)
    with
        | Sys_error msg -> print_endline
            ("Packed: Could not install the package's files: " ^ msg); false
        | _ -> print_endline ("Packed: Could not install the package's files"); false