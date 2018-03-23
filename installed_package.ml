open Util
open Pkg
open Status

let list_installed_packages () =
    print_target ();
    match read_status () with None -> false | Some status ->
    List.iter
        (fun (pkg,_,_) -> print_endline (string_of_pkg pkg))
        status;
    true

let get_dirty_packages status print =
    let _,pps =
        (* Accumulator: (double counter, problematic) *)
        List.fold_left
            (fun (cts,pps) t ->
                let (pkg, preason, pstate) = t
                in
                let g =
                if pstate <> Installed
                then
                    ((if print
                    then
                        print_endline ("\"" ^ (string_of_pkg pkg) ^
                            "\" is in the following dirty state: " ^
                            (string_of_installation_status pstate))
                    else
                        ());
                    false)
                else true
                in
                let g =
                    if (List.exists (fun e -> compare_tupels e t = 0) cts)
                    then
                        ((if print
                        then
                            print_endline ("Duplicate package: \"" ^
                                (string_of_pkg pkg) ^ "\"")
                        else ());
                        false)
                    else
                        g
                in
                (t::cts, if g then pps else pkg::pps))
            ([],[])
            status
    in
    pps

let check_installation status print =
    get_dirty_packages status print = []

let show_problems_with_installation () =
    print_target ();
    match read_status () with None -> false | Some status ->
    match check_installation status true with
        | true -> print_endline "No problems detected"; true
        | false -> print_endline "Problems found"; false

let force_remove status name =
    match select_status_tupel_by_name status name with
        | None -> print_endline ("Force_Remove: Package \"" ^ name ^
            "\" is not installed"); None
        | Some (pkg, preason, pstate) ->
    print_endline ("Force removing package \"" ^ name ^ "\":");

    let pkg_info_location =
        form_target_path (Tpm_config.package_info_location ^ "/" ^ name)
    in

    print_string "    Determining wich files must be removed";
    let rmfiles = pkg.files
    in
    let rmfiles =
        List.fold_left
            (fun fs (cks, n) ->
                match sha512sum_of_file_opt (form_target_path n) with
                    | None -> fs
                    | Some rs -> if rs = cks then n::fs
                        else
                        (print_newline ();
                        print_string ("    not removing config file \"" ^
                            n ^ "\" because it was modified");
                        fs))
            (* The config files shall be removed last. Hence reverse the list. *)
            (List.rev rmfiles)
            pkg.cfiles
    in
    (* Reverse the list a second time *)
    let rmfiles = List.rev rmfiles
    in
    print_ok ();    
    
    print_string "    Marking removal in status";
    let status = update_status_tupel status (pkg, preason, Removal)
    in
    if write_status status |> not then (print_failed (); None)
    else
    (print_ok ();

    let exec_prerm_script status =
        let prermsh_tmp_path =
            Tpm_config.tmp_dir ^ "/" ^ Tpm_config.prermsh_name
        in
        let prermshgz_path =
            pkg_info_location ^ "/" ^ Tpm_config.prermsh_name ^ ".gz"
        in
        let exec status =
            match status with None -> None | Some status ->
            print_string "    Executing the prerm script";
            match create_tmp_dir () with false -> None | true ->
            let unzip_prermsh_cmd =
                !program_gzip ^ " -cd " ^ prermshgz_path ^ " > " ^
                prermsh_tmp_path
            in
            try
                if Sys.command unzip_prermsh_cmd = 0
                then
                    (Unix.chmod prermsh_tmp_path 0o755;
                    if Sys.command prermsh_tmp_path = 0
                    then (print_ok (); Some status)
                    else (print_failed (); Some status))
                else
                    (print_newline (); print_string
                    "    unzipping the prerm script failed";
                    print_failed (); Some status)
            with
                | Unix.Unix_error (c,_,_) -> print_newline (); print_string
                    ("    " ^ Unix.error_message c); print_failed ();
                    Some status
                | Sys_error msg -> print_newline (); print_string
                    ("    " ^ msg); print_failed (); Some status
                | _ -> print_failed (); Some status
        in
        let find_prerm_script status =
            match status with None -> None | Some status ->
            print_string "    Looking for a prerm script";
            match file_status prermshgz_path with
                | Read_error -> print_newline (); print_string
                    ("    can not read \"" ^ prermshgz_path ^ "\"");
                    print_failed (); Some status
                | Directory -> print_newline (); print_string
                    ("    \"" ^ prermshgz_path ^ "\"is a directory");
                    print_failed (); Some status
                | Non_existent -> print_ok (); print_endline
                    "    This package has no prerm script"; Some status
                | Other_file ->
                    print_ok (); exec (Some status)
        in
        status |> find_prerm_script
    in
    match exec_prerm_script (Some status) with None -> None | Some status ->

    let remove_file n =
        try
            Unix.unlink n;
            true
        with
            | Unix.Unix_error (Unix.ENOENT,_,_) -> true
            | Unix.Unix_error (c,_,_) -> print_newline (); print_string
                ("    failed to remove \"" ^ n ^ "\": " ^
                (Unix.error_message c)); false
            | _ -> print_newline ();
                print_string ("    failed to remove \"" ^ n ^
                "\""); false
    in
    let remove_directory n =
        try
            Unix.rmdir n;
            true
        with
            | Unix.Unix_error (Unix.ENOENT,_,_)
            | Unix.Unix_error (Unix.ENOTEMPTY,_,_) -> true
            | Unix.Unix_error (c,_,_) -> print_newline(); print_string
                ("    failed to remove directory \"" ^ n ^ "\": " ^
                (Unix.error_message c)); false
            | _ -> print_newline (); print_string
                ("    failed to remove directory \"" ^ n ^ "\""); false
    in

    print_string "    Removing files";
    let s =
        List.fold_left
            (fun s fn ->
                let fn = form_target_path fn
                in
                remove_file fn && s)
            true
            rmfiles
    in
    if not s then (print_failed (); None)
    else
    (print_ok ();

    print_string "    Removing directories";
    let s =
        List.fold_left
            (fun s dn ->
                let dn = form_target_path dn
                in
                remove_directory dn && s)
            true
            (* The directory list is sorted therefore processing it in reverse
             * order means processing from inner to outer. *)
            (List.rev pkg.dirs)
    in
    if not s then (print_failed (); None)
    else
    (print_ok ();

    let remove_pkg_info_location status =
        match status with None -> None | Some status ->
        print_string "    Removing the package info location";
        match file_status pkg_info_location with
            | Non_existent -> Some status
            | _ ->
                if rmdir_r pkg_info_location
                then (print_ok (); Some status)
                else (print_failed (); None)
    in
    let remove_from_status status =
        match status with None -> None | Some status ->
        print_string "    Removing package from status";
        let status = delete_status_tupel status (pkg, preason, Removal)
        in
        if write_status status |> not then (print_failed (); None)
        else (print_ok (); Some status)
    in
    Some status
    |> remove_pkg_info_location
    |> remove_from_status
    )
    )
    )