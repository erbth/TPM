open Util
open Pkg
open Repository

let read_package repo name version arch =
    if not (create_tmp_dir ()) then None
    else
    match provide_transport_shape repo name version arch with
        | None -> None
        | Some path ->
    let unpack_args =
        [|!program_tar; "-xf"; path;
        "-C"; Tpm_config.tmp_dir; Tpm_config.desc_file_name|]
    in
    try
        match run_program unpack_args with
            | (_, WEXITED 0) ->
                Xml.parse_file
                    (Tpm_config.tmp_dir ^ "/" ^ Tpm_config.desc_file_name)
                |> pkg_of_xml
            | _ -> print_endline
                "Packed: Could not unpack the package (tar failed)";
                None
    with
        | Unix.Unix_error (c, _, _) -> print_endline
            ("Packed: Could not read the package: " ^ Unix.error_message c);
            None
        | _ -> print_endline ("Packed: Could not read the package"); None

let unpack_packed_shape repo name version arch =
    if not (create_tmp_dir ()) then false
    else
    match provide_transport_shape repo name version arch with
        | None -> false
        | Some path ->
    let unpack_package_args =
        [|!program_tar; "-xf"; path; "-C"; Tpm_config.tmp_dir|]
    in
    try
        match run_program unpack_package_args with
            | (_, WEXITED 0) -> true
            | _ -> print_endline
                "Packed: Could not unpack the package (tar failed)";
                false
    with
        | Unix.Unix_error (c, _, _) -> print_endline
            ("Packed: Failed to unpack the package: " ^ Unix.error_message c);
            false
        | _ -> print_endline "Packed: Failed to unpack the package"; false

let unpack_files_to spkg destination exclude_files status =
    let exclude_files =
        List.map
            (fun fn ->
                let fn =
                    if String.length fn > 0
                    then
                        String.sub fn 1 (String.length fn - 1)
                    else fn
                in
                "--exclude=" ^ fn)
            exclude_files
        |>
        Array.of_list
    in
    let unpack_files_args =
        Array.append
            [|!program_tar;
            "-xpI"; !program_gzip;
            "-f"; Tpm_config.tmp_dir ^ "/" ^ Tpm_config.destdir_name ^ ".tar.gz";
            "-C"; destination|]
            exclude_files
    in
    try
        if
            try
                match run_program unpack_files_args with
                    | (_, WEXITED 0) -> true
                    | _ -> false
            with
                _ -> false
        then true
        else
            (print_endline
                "Packed: Could not install the package's files (tar failed)";
            false)
    with
        | Sys_error msg -> print_endline
            ("Packed: Could not install the package's files: " ^ msg); false
        | _ -> print_endline ("Packed: Could not install the package's files"); false
