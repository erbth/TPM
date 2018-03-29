open Util
open Pkg
open Repository
open Repository_search
open Status
open Packed_package
open Installed_package
open Configuration

let install_package status repo pkg reason =
    match (pkg.t, pkg.n) with
        | (_, None)
        | (None, _) -> print_endline "Installation: Invalid package"; None
        | (Some pkg_type, Some pkg_name) ->

    print_endline ("Installing \"" ^ (string_of_pkg pkg) ^ "\"=" ^
        (match pkg.v with None -> "?" | Some v -> string_of_version v) ^
        " (" ^ string_of_pkg_type pkg_type ^ ") " ^
        " from " ^ (string_of_repository repo));

    let package_info_location =
        form_target_path (Tpm_config.package_info_location ^ "/" ^ pkg_name)
    in

    let conf_determine_files_to_exclude pkg status =
        (status, [])
    in

    let sw_determine_files_to_exclude pkg status =
        match status with None -> None, [] | Some status ->
        print_string "    Determining wich config files must be excluded";
        match
            List.fold_left
                (fun (status, efs) (_, cf) ->
                    match status with None -> (None, []) | Some status ->
                    match file_status (form_target_path cf) with
                        | Other_file -> print_newline (); print_string
                            ("    config file \"" ^ cf ^ "\" does already " ^
                            "exist, hence not installing it");
                            (Some status, cf::efs)
                        | Non_existent -> (Some status, efs)
                        | Directory -> print_newline (); print_string
                            ("    config file \"" ^ cf ^
                            "\" does already exist as directory");
                            (None, [])
                        | Read_error -> print_newline (); print_string
                            ("    read error while testing for config file \"" ^
                            cf ^ "\"");
                            (None, []))
                (Some status, [])
                (pkg.cfiles)
        with
            | (None, _) -> print_failed (); (None, [])
            | (Some status, l) -> print_ok (); (Some status, l)
    in

    let determine_files_to_exclude pkg_type status =
        match status with None -> None, [] | Some status ->
        (match pkg_type with
            | Conf -> conf_determine_files_to_exclude pkg (Some status)
            | Sw -> sw_determine_files_to_exclude pkg (Some status))
    in

    let check_for_file_collisions (status, excluded_files) =
        match status with None -> None, [] | Some status ->
        print_string "    Checking if any of the package's files exists already";
        let files_to_install =
            List.fold_left
                (fun fs (_,cf) -> cf::fs)
                (List.rev pkg.files)
                pkg.cfiles
            |> List.rev
            |> List.filter
                (fun f -> not (List.exists (fun ef -> f = ef) excluded_files))
        in
        match
            List.exists
                (fun fn ->
                    match file_status (form_target_path fn) with
                        | Directory
                        | Other_file -> print_newline (); print_string
                            ("    \"" ^ fn ^ "\" exists already"); true
                        | Read_error -> print_newline (); print_string
                            ("    can not check for \"" ^ fn ^ "\""); true
                        | Non_existent -> false)
                files_to_install
        with
            | true -> print_failed (); None, []
            | false -> print_ok (); Some status, excluded_files
    in

    let tmp_dir (status, excluded_files) =
        match status with None -> None, [] | Some status ->
        print_string "    Creating a temporary directory";
        if create_tmp_dir ()
        then (print_ok (); Some status, excluded_files)
        else (print_failed (); None, excluded_files)
    in

    let gain_access (status, excluded_files) =
        match status with None -> (None, [], "") | Some status ->
        print_string "    Gaining access to the package";
        match provide_transport_shape repo pkg with
            | None -> print_failed (); (None, [], "")
            | Some path -> print_ok (); (Some status, excluded_files, path)
    in

    let mark_change (status, excluded_files, tpath) =
        match status with None -> None, [], "" | Some status ->
        print_string "    Marking the change in status";
        let status = unique_insert_status_tupel status (pkg, reason, Installation)
        in
        if write_status status |> not then (print_failed (); None, [], "")
        else (print_ok (); Some status, excluded_files, tpath)
    in

    let create_pkg_info_location pkg_name (status, excluded_files, tpath) =
        match status with None -> None, [], "" | Some status ->
        print_string "    Creating the package info location";
        try
            mkdir_p_at_target
                (Tpm_config.package_info_location ^ "/" ^ pkg_name)
                0o755;
            print_ok ();
            Some status, excluded_files, tpath 
        with
            | Unix.Unix_error (c,_,_) -> print_newline (); print_string
                ("    " ^ Unix.error_message c); print_failed (); None, [], ""
            | _ -> print_failed (); None, [], ""
    in

    let install_files pkg (status, excluded_files, tpath) =
        match status with None -> None, "" | Some status ->
        print_string "    installing files";
        
        if unpack_files repo pkg excluded_files |> not
        then (print_failed (); None, "")
        else (print_ok (); Some status, tpath)
    in

    let exec_postinstsh (status, tpath) =
        match status with None -> None, "" | Some status ->
        print_string "    Looking for a postinst script";
        let unpack_postinst_cmd =
            !program_tar ^ " -xf " ^ tpath ^ " -C " ^
            Tpm_config.tmp_dir ^ " " ^ Tpm_config.postinstsh_name ^ " 2>&1"
        in
        let hp =
            try
                let ic = Unix.open_process_in unpack_postinst_cmd
                in
                Some (Unix.close_process_in ic = Unix.WEXITED 0)
            with
                | Unix.Unix_error (c,_,_) -> print_newline(); print_string
                    ("    testing for a postinst script failed: " ^
                    (Unix.error_message c)); None
                | _ -> print_newline (); print_string
                    ("    testing for a postinst script failed"); None
        in
        match hp with None -> print_failed (); None, "" | Some hp ->
        print_ok ();
        let s =
            (if hp
            then
                (print_string "    Executing the postinst script";
                if
                    try
                        let postinstsh =
                            Tpm_config.tmp_dir ^ "/" ^ Tpm_config.postinstsh_name
                        in
                        Unix.chmod postinstsh 0o755;
                        Sys.command postinstsh = 0
                    with
                        | Unix.Unix_error (c,_,_) -> print_newline (); print_string
                            ("    " ^ Unix.error_message c); false
                        | Sys_error msg -> print_newline (); print_string
                            ("    " ^ msg); false
                        | _ -> false
                then (print_ok (); true)
                else (print_failed (); false))
            else
                (print_endline "    This package has no postinst script"; true))
        in
        if not s then None, ""
        else Some status, tpath
    in

    let copy_prermsh (status, tpath) =
        match status with None -> None, "" | Some status ->
        print_string "    Copying files to the package info location";
        let unpack_cmd =
            !program_tar ^ " -xf " ^ tpath ^
            " -C " ^ Tpm_config.tmp_dir ^ " " ^ Tpm_config.prermsh_name ^
            " > /dev/zero 2>&1"
        in
        let compress_cmd =
            !program_gzip ^ " -c " ^
            Tpm_config.tmp_dir ^ "/" ^ Tpm_config.prermsh_name ^
            " > " ^
            package_info_location ^ "/" ^ Tpm_config.prermsh_name ^ ".gz"
        in
        try
            if Sys.command unpack_cmd <> 0
            then
                (print_ok (); Some status, tpath)
            else
                let pu = Unix.umask 0o022
                in
                let status =
                    if Sys.command compress_cmd <> 0
                    then
                        (print_newline (); print_string "    Gzip failed";
                        print_failed (); None)
                    else Some status
                in
                let _ = Unix.umask pu
                in
                print_ok (); status, tpath
        with
            | Sys_error msg -> print_newline (); print_string ("    " ^ msg);
                print_failed (); None, ""
            | Unix.Unix_error (c,_,_) -> print_newline (); print_string
                ("    " ^ Unix.error_message c); print_failed (); None, ""
            | _ -> print_failed (); None, ""
    in

    let acknowledge_change (status, tpath) =
        match status with None -> None | Some status ->
        print_string "    Acknowledging the change in status";
        let status = update_status_tupel status (pkg, reason, Installed)
        in
        if write_status status
        then (print_ok (); Some status)
        else (print_failed (); None)
    in
    
    Some status
    |> determine_files_to_exclude pkg_type
    |> check_for_file_collisions
    |> tmp_dir
    |> gain_access
    |> mark_change
    |> create_pkg_info_location pkg_name
    |> install_files pkg
    |> copy_prermsh
    |> exec_postinstsh
    |> acknowledge_change

let upgrade_package status repo pkg =
    print_endline ("Upgrade: not supported yet (package \"" ^
        (string_of_pkg pkg) ^ "\""); None

let install_or_upgrade_package status repo pkg reason =
    match (pkg.t, pkg.n, pkg.v, pkg.a) with
        | (Some t, Some n, Some v, Some a) ->
            (match select_status_tupel_by_name status n with
                | None -> install_package status repo pkg reason
                | Some (ipkg,_,_) ->
                    if ipkg.v <> Some v then upgrade_package status repo pkg
                    else (print_endline ("\"" ^ n ^ "\"=" ^ (string_of_version v) ^
                        " is already installed."); Some status))
        | _ ->
            print_endline "Installation: Invalid package"; None

let remove_package status name =
    match select_status_tupel_by_name status name with
        | None -> print_endline ("Removal: Package \"" ^ name ^
            "\" is not installed"); None
        | Some (pkg, preason, pstate) ->
    print_endline ("Removing package \"" ^ name ^ "\":");

    let pkg_info_location =
        form_target_path (Tpm_config.package_info_location ^ "/" ^ name)
    in

    print_string "    Determining wich files must be removed";
    let rmfiles = pkg.files
    in
    let rmfiles =
        List.fold_left
            (fun fs (cks, n) ->
                match fs with None -> None | Some fs ->
                match sha512sum_of_file_opt (form_target_path n) with
                    | None -> print_newline (); print_string
                        ("    failed to calculate sha512sum of file \"" ^
                        n ^ "\""); None
                    | Some rs -> if rs = cks then Some (n::fs)
                        else
                        (print_newline ();
                        print_string ("    not removing config file \"" ^
                            n ^ "\" because it was modified");
                        Some fs))
            (* The config files shall be removed last. Hence reverse the list. *)
            (Some (List.rev rmfiles))
            pkg.cfiles
    in
    match rmfiles with None -> print_failed (); None | Some rmfiles ->
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

    print_string "    Looking for a prerm script";
    let prermsh_tmp_path =
        Tpm_config.tmp_dir ^ "/" ^ Tpm_config.prermsh_name
    in
    let prermshgz_path =
        pkg_info_location ^ "/" ^ Tpm_config.prermsh_name ^ ".gz"
    in
    let s =
        match file_status prermshgz_path with
            | Read_error -> print_newline (); print_string
                ("    can not read \"" ^ prermshgz_path ^ "\"");
                print_failed (); false
            | Directory -> print_newline (); print_string
                ("    \"" ^ prermshgz_path ^ "\"is a directory");
                print_failed (); false
            | Non_existent -> print_ok (); print_endline
                "    This package has no prerm script"; true
            | Other_file ->
                print_ok ();
                print_string "    Executing the prerm script";
                match create_tmp_dir () with false -> false | true ->
                let unzip_prermsh_cmd =
                    !program_gzip ^ " -cd " ^ prermshgz_path ^ " > " ^
                    prermsh_tmp_path
                in
                try
                    if Sys.command unzip_prermsh_cmd = 0
                    then
                        (Unix.chmod prermsh_tmp_path 0o755;
                        if Sys.command prermsh_tmp_path = 0
                        then (print_ok (); true)
                        else (print_failed (); false))
                    else
                        (print_newline (); print_string
                        "    unzipping the prerm script failed";
                        print_failed (); false)
                with
                    | Unix.Unix_error (c,_,_) -> print_newline (); print_string
                        ("    " ^ Unix.error_message c); print_failed (); false
                    | Sys_error msg -> print_newline (); print_string
                        ("    " ^ msg); print_failed (); false
                    | _ -> print_failed (); false
    in
    match s with false -> None | true ->

    let remove_file n =
        try
            Sys.remove n;
            true
        with
            | Sys_error msg -> print_newline ();
                print_string ("    failed to remove \"" ^
                n ^ "\": " ^ msg); false
            | _ -> print_newline ();
                print_string ("    failed to remove \"" ^ n ^
                "\""); false
    in
    let remove_directory n =
        try
            Unix.rmdir n;
            true
        with
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
                match file_status fn with
                    | Other_file -> remove_file fn && s
                    | Non_existent -> (print_newline (); print_string
                        ("    \"" ^ fn ^
                        "\" does not exist, hence not removing it"); s)
                    | Read_error -> print_newline (); print_string
                        ("    can not read \"" ^ fn ^ "\""); false
                    | Directory -> print_newline (); print_string
                        ("    \"" ^ fn ^ "\" is a directory"); false)
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
                match file_status dn with
                    | Directory ->
                        (try
                            if Sys.readdir dn |> array_is_empty
                            then
                                remove_directory dn && s
                            else
                                (print_newline ();
                                print_string ("    \"" ^ dn ^
                                    "\" is not empty hence not removing it"); s)
                        with
                            | Sys_error msg -> print_newline ();
                                print_string msg; false
                            | _ -> print_newline ();
                                print_endline ("    can not read from \"" ^ dn ^
                                    "\""); false)
                    | Other_file -> print_newline (); print_string
                        ("    \"" ^ dn ^ "\" is not a directory"); false
                    | Non_existent -> print_newline (); print_string
                        ("    \"" ^ dn ^ "\" does not exist hence not removing it");
                        s
                    | Read_error -> print_newline (); print_string
                        ("    can not read from \"" ^ dn ^ "\""); false)
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

let show_policy name =
    print_target ();
    match read_configuration () with
        | None -> false
        | Some cfg ->
    let rps =
        find_package_in_all_repos name |>
        List.filter (fun rp ->
            let (_,p) = rp
            in p.Pkg.a = Some cfg.Configuration.a)
    in
    let rpti = select_version_to_install rps
    in
    match read_status () with None -> false | Some status ->
    let i_pkg =
        select_status_tupel_by_name status name
    in
    let string_of_rp (r,p) =
        (match p.v with None -> "???" | Some v -> string_of_version v) ^
        " @ " ^
        (match p.a with None -> "???" | Some a -> string_of_arch a) ^
        " from " ^ (string_of_repository r)
    in
    let string_of_it (p,r,_) =
        (match p.v with None -> "???" | Some v -> string_of_version v) ^
        " @ " ^
        (match p.a with None -> "???" | Some a -> string_of_arch a) ^
        " (" ^ (string_of_installation_reason r) ^ ")"
    in
    print_endline ("Policy for package \"" ^ name ^ "\":");
    print_endline ("  Installed instance:  " ^
        match i_pkg with
            | None -> "---"
            | Some t -> string_of_it t);
    print_endline ("  Instance to install: " ^
        match rpti with
            | None -> "---"
            | Some rp -> string_of_rp rp);
    print_endline "  All available instances:";
    List.iter (fun rp -> print_endline ("    " ^ string_of_rp rp)) rps;
    true

