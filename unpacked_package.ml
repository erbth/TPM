open Util
open Pkg

(* Returns true on success and false otherwise *)
let create_package t =
    if Sys.file_exists Tpm_config.desc_file_name
    then
        (print_endline ("\"" ^ Tpm_config.desc_file_name ^ "\" exists already."); false)
    else
    match pkg_type_of_string t with
        | None -> print_endline "Invalid package type"; false
        | Some t ->
            let s =
                try
                    Unix.mkdir Tpm_config.destdir_name 0o755;
                    Unix.chown Tpm_config.destdir_name 0 0; true
                with
                    | Unix.Unix_error (c,_,_) -> print_endline
                        ("Could not create directory: " ^ Unix.error_message c);
                        false
                    | _ -> print_endline "Could not create directory"; false
            in
            if not s then false
            else
                let pkg = { empty_pkg with t = Some t }
                in
                write_package pkg


let show_missing_information () =
    match read_package () with
        | None -> false
        | Some pkg ->
            let {t=t;n=n;v=v;a=a;files=files} = pkg
            in
            ((if t = None then print_endline "The package's type is missing" else ());
            (if n = None then print_endline "The package's name is missing" else ());
            (if v = None then print_endline "The package's version is missing" else ());
            (if a = None then print_endline "The package's architecture is missing" else ());
            (if files = [] then print_endline
                "Warning: the package does not contain any files" else ());
            if information_missing pkg then false
            else
                (print_endline "All essential information is present."; true))

let set_package_name n =
    match read_package () with
        | None -> false
        | Some pkg -> write_package {pkg with n = Some n}

let set_package_version v =
    match version_of_string v with
        | None -> print_endline "Invalid version"; false
        | Some v ->
            match read_package () with
                | None -> false
                | Some pkg -> write_package {pkg with v = Some v}

let set_package_architecture a =
    match arch_of_string a with
        | None -> print_endline "Invalid architecture"; false
        | Some a ->
            match read_package () with
                | None -> false
                | Some pkg -> write_package {pkg with a = Some a}

let add_files_from_destdir () =
    let process_file n p =
        try
            let cmd = Tpm_config.program_sha512sum ^ " " ^ n
            in
            let ic = Unix.open_process_in cmd
            in
            let sha512sum = input_line ic
            in
            if Unix.close_process_in ic <> Unix.WEXITED 0
            then
                (print_endline (cmd ^ " failed."); None)
            else
                let sha512sum = String.split_on_char ' ' sha512sum |> List.hd
                in
                    Some (sha512sum, p)
        with
            | Unix.Unix_error (c,_,_) ->
                print_endline ("Calculating the sha512 sum of file \"" ^
                    n ^ "\" failed: " ^ Unix.error_message c ^ "."); None
            | _ -> print_endline ("Calculating the sha512 sum of file \"" ^
                    n ^ "\" failed."); None
    in
    let rec process_dir fs d p =
        try
            let c = Sys.readdir d |> Array.to_list |> List.sort compare_names
            in
                (List.fold_left (fun a f ->
                        match a with None -> None | Some a ->
                        let p = p ^ "/" ^ f
                        in
                        let f = d ^ "/" ^ f
                        in
                        if Sys.is_directory f
                        then
                            process_dir a f p
                        else
                            match process_file f p with
                                | None -> None
                                | Some f -> Some (f :: a)
                        )
                    (Some fs)
                    c)
        with
            | Sys_error m ->
                print_endline ("Reading directory \"" ^ d ^ "\" failed: " ^ m ^ ".");
                None
            | _ ->
                print_endline ("Reading directory \"" ^ d ^ "\" failed.");
                None
    in
    match read_package () with
        | None -> false
        | Some pkg ->
            if not (Sys.is_directory Tpm_config.destdir_name)
            then (print_endline ("\"" ^ Tpm_config.destdir_name ^ "\" is not a directory.");
                false)
            else
                match process_dir [] Tpm_config.destdir_name "" with
                    | None -> false
                    | Some fs ->
                        let fs = List.rev fs
                        in
                        let cfs = List.filter
                            (fun f -> let (_,n) = f in contains
                                Tpm_config.conf_path_prefixes
                                (String.sub n 0 3))
                            fs
                        in
                        let ncfs = sorted_difference compare_names fs cfs
                        in write_package {pkg with files = ncfs; cfiles = cfs}

let add_runtime_dependency d =
    match read_package () with
        | None -> false
        | Some pkg ->
            let rdeps = List.merge compare_names pkg.rdeps [d]
            in
                write_package {pkg with rdeps = rdeps}

let remove_runtime_dependencies () =
    match read_package () with
        | None -> false
        | Some pkg -> write_package {pkg with rdeps = []}

let create_packed_form () =
    let pack archivename =
        let pack_destdir_cmd =
            Tpm_config.program_cd ^ " " ^ Tpm_config.destdir_name ^ " && " ^
            Tpm_config.program_tar ^ " -cpI " ^  Tpm_config.program_gzip ^
            " -f ../" ^ Tpm_config.destdir_name ^ ".tar.gz *"
        in
        let pack_package_cmd =
            Tpm_config.program_tar ^ " -cf " ^ archivename ^ " " ^
            Tpm_config.desc_file_name ^ " " ^ Tpm_config.destdir_name ^ ".tar.gz"
        in
        try
            if Sys.command pack_destdir_cmd <> 0
            then failwith "Compressing destdir failed"
            else
            if Sys.command pack_package_cmd <> 0
            then failwith "Creating the package archive failed"
            else true
        with
            | Sys_error m | Failure m ->
                print_endline ("Packing the package failed: " ^ m ^ "."); false
            | _ ->
                print_endline "Packing the package failed."; false

    in
    match read_package () with
        | None -> false
        | Some pkg ->
            if information_missing pkg
            then (print_endline "Information is missing."; false)
            else
            match (pkg.n, pkg.v, pkg.a) with
                | (Some n, Some v, Some a) ->
                    let archivename = n ^ "-" ^ (string_of_version v) ^ "_" ^
                        (string_of_arch a) ^ ".tpm.tar"
                    in
                        pack archivename
                | _ -> print_endline "No idea what's going on ... Aborting."; false
