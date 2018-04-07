open Util
open Pkg
open Repository
open Repository_search
open Status
open Packed_package
open Installed_package
open Configuration
open Depres

(* Filters that are useful for constructing pipelines of operations to
 * install and remove packages *)
let create_tmp_dir_filter status =
    match status with None -> None | Some status ->
    print_string "    Creating a temporary directory";
    if create_or_clean_tmp_dir ()
    then (print_ok (); Some status)
    else (print_failed (); None)

let unpack_package_filter repo pkg status =
    match status with None -> None | Some status ->
    print_string "    Unpacking the package";
    match unpack_packed_form repo pkg with
        | false -> print_failed (); None
        | true -> print_ok (); Some status

let check_for_file_collisions_filter spkg status =
    match status with None -> None | Some status ->
    print_string "    Checking if any of the package's files exist already";
    match
        List.fold_left
            (fun s fn ->
                match file_status (form_target_path fn) with
                    | Directory
                    | Other_file -> print_newline (); print_string
                        ("    \"" ^ fn ^ "\" exists already"); None
                    | Read_error -> print_newline (); print_string
                        ("    can not check for \"" ^ fn ^ "\""); None
                    | Non_existent -> s)
            (Some status)
            spkg.sfiles
    with
        | None -> print_failed (); None
        | Some status -> print_ok (); Some status

let install_determine_files_to_exclude_filter spkg status =
    let install_conf_determine_files_to_exclude spkg status =
            (status, [])
    in
    let install_sw_determine_files_to_exclude spkg status =
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
                (spkg.scfiles)
        with
            | (None, _) -> print_failed (); (None, [])
            | (Some status, l) -> print_ok (); (Some status, l)
    in
    match status with None -> None, [] | Some status ->
    (match spkg.st with
        | Conf -> install_conf_determine_files_to_exclude spkg (Some status)
        | Sw -> install_sw_determine_files_to_exclude spkg (Some status))

let unpack_files_filter pkg (status, excluded_files) =
    match status with None -> None | Some status ->
    (* This flushes stdout *)
    print_endline "    Unpacking files";
    print_string  "                   ";
    
    if unpack_files_from_tmp pkg excluded_files
    then (print_ok (); Some status)
    else (print_failed (); None)

let create_pkg_info_location_filter spkg status =
    match status with None -> None | Some status ->
    print_string "    Creating the package info location";
    try
        mkdir_p_at_target
            (Tpm_config.package_info_location ^ "/" ^ spkg.sn)
            0o755;
        print_ok ();
        Some status 
    with
        | Unix.Unix_error (c,_,_) -> print_newline (); print_string
            ("    " ^ Unix.error_message c); print_failed (); None
        | _ -> print_failed (); None

let copy_packaging_scripts_filter spkg status =
    match status with None -> None | Some status ->
    print_string "    Copying files to the package info location";
    let package_info_location =
        form_target_path (Tpm_config.package_info_location ^ "/" ^ spkg.sn)
    in
    match
        try
            Some (List.fold_left
                (fun c s ->
                    let s = Tpm_config.tmp_dir ^ "/" ^ s
                    in
                    if Sys.file_exists (s)
                    then (c ^ " " ^ s)
                    else c)
                ""
                all_packaging_scripts)
        with
            | Sys_error msg -> print_newline (); print_string msg; None
            | _ -> None
    with
        | None -> print_failed (); None
        | Some "" -> print_ok (); Some status
        | Some pkg_scripts ->

    let install_cmd =
        !program_install ^ " -m755" ^
        pkg_scripts ^ " " ^
        package_info_location ^ "/"
    in
    try
        if Sys.command install_cmd = 0
        then
            (print_ok (); Some status)
        else
            (print_failed (); None)
    with
        | Sys_error msg -> print_newline (); print_string ("    " ^ msg);
            print_failed (); None
        | _ -> print_failed (); None

let execute_packaging_script_filter spkg scriptname status =
    match status with None -> None | Some status ->
    let script =
        Tpm_config.package_info_location ^ "/" ^ spkg.sn ^ "/" ^
        scriptname
        |> form_target_path
    in
    try 
        if Sys.file_exists script
        then
            (* This flushes the standard output *)
            (print_endline ("    Executing " ^ scriptname);
            print_string    "                               ";
            if Sys.command
                (!program_cd ^ " " ^ !target_system ^ " && " ^ script)
                = 0
            then (print_ok (); Some status)
            else (print_failed (); None))
        else
            (print_endline ("    This package has no " ^ scriptname ^ ".");
            Some status)
    with
        | Sys_error msg -> print_endline
            ("    Executing " ^ scriptname ^ " failed: " ^ msg);
            print_failed ();
            None
        | _ -> print_endline ("    Executing " ^ scriptname ^ " failed");
            print_failed ();
            None

let reset_configured_dependent_packages_filter name status =
    match status with None -> None | Some status ->
    print_endline "    Resetting dependent packages to state installed";
    let configured_dep_names =
        get_dependent_package_names
            (fun (p,r,s) -> is_configured_of_state s)
            status
            name
    in
    List.fold_left
        (fun status n ->
            match status with None -> None | Some status ->
            match select_status_tuple_by_name status n with
                | None -> None
                | Some (p,r,s) ->
                    print_endline ("        " ^ string_of_pkg p);
                    Some (update_status_tuple status (p, r, Installed)))
        (Some status)
        configured_dep_names
    |> (fun s ->
        print_string "        ";
        match s with
            | Some s -> print_ok (); Some s
            | None -> print_failed (); None)

(* Install a package from a specific repository and mark it with a specific
 * reason for installing it. *)
let install_package status repo pkg reason =
    match static_of_dynamic_pkg pkg with
        | None -> print_endline "Installation: Invalid package"; None
        | Some spkg ->

    print_endline ("Installing \"" ^ (string_of_pkg pkg) ^ "\"=" ^
        (match pkg.v with None -> "?" | Some v -> string_of_version v) ^
        " (" ^ string_of_pkg_type spkg.st ^ ") " ^
        " from " ^ (string_of_repository repo));

    let install_mark_change_filter pkg reason (status, excluded_files) =
        match status with None -> None, [] | Some status ->
        print_string "    Marking the change in status";
        let status = unique_insert_status_tuple status (pkg, reason, Installation)
        in
        if write_status status |> not then (print_failed (); None, [])
        else (print_ok (); Some status, excluded_files)
    in

    let install_commit_changes_filter status =
        match status with None -> None | Some status ->
        print_string "    Commiting changes to status";
        let status = update_status_tuple status (pkg, reason, Installed)
        in
        if write_status status
        then (print_ok (); Some status)
        else (print_failed (); None)
    in
    
    Some status
    |> create_tmp_dir_filter
    |> unpack_package_filter repo pkg
    |> check_for_file_collisions_filter spkg
    |> install_determine_files_to_exclude_filter spkg
    |> install_mark_change_filter pkg reason
    |> unpack_files_filter pkg
    |> create_pkg_info_location_filter spkg
    |> copy_packaging_scripts_filter spkg
    |> execute_packaging_script_filter spkg Tpm_config.postinstsh_name
    |> reset_configured_dependent_packages_filter spkg.sn
    |> install_commit_changes_filter

let upgrade_package status repo pkg =
    print_endline ("Upgrade: not supported yet (package \"" ^
        (string_of_pkg pkg) ^ "\""); None

let install_or_upgrade_package status repo pkg reason =
    match (pkg.t, pkg.n, pkg.v, pkg.a) with
        | (Some t, Some n, Some v, Some a) ->
            (match select_status_tuple_by_name status n with
                | None -> install_package status repo pkg reason
                | Some (ipkg,preason,pstatus) ->
                    if ipkg.v <> Some v then upgrade_package status repo pkg
                    else
                    match reason with
                        | Auto -> Some status
                        | Manual ->
                            if preason = Manual
                            then (print_endline
                                ("\"" ^ n ^ "\"=" ^ (string_of_version v) ^
                                " is already installed."); Some status)
                            else (print_string
                                ("\"" ^ n ^ "\"=" ^ (string_of_version v) ^
                                " was automatically installed already, " ^
                                "marking it as manually installed");
                                let status =
                                    update_status_tuple_installation_reason
                                        status n Manual
                                in
                                match
                                    write_status status
                                with
                                    | true -> print_ok (); Some status
                                    | false -> print_failed (); None))
        | _ ->
            print_endline "Installation: Invalid package"; None

let configure_package status name =
    print_endline ("Configuring package \"" ^ name ^ "\"");

    let configure_check_target_filter status =
        print_string "    Checking if the target system is \"/\"";
        match !target_system with
            | "/" -> print_ok (); Some status
            | _ -> print_failed (); None
    in
    let configure_mark_change_filter name status =
        match status with None -> (None, None) | Some status ->
        print_string "    Marking change in status";
        match select_status_tuple_by_name status name with
            | None -> print_newline (); print_string
                ("Configure: Package \"" ^ name ^ "\" is not installed.");
                print_failed (); (None, None)
            | Some (p,r,s) when s = Installed ->
                (let status = update_status_tuple status (p,r,Configuration)
                in
                match write_status status with
                    | true -> print_ok (); (Some status, static_of_dynamic_pkg p)  
                    | false -> print_failed (); (None, None))
            | _ -> print_newline (); print_string
                ("Configure: Package \"" ^ name ^ "\" is not in state installed");
                print_failed (); (None, None)
    in
    let configure_confirm_change_filter name status =
        match status with None -> None | Some status ->
        print_string "    Confirming change in status";
        match select_status_tuple_by_name status name with
            | None -> print_failed (); None
            | Some (p,r,s) ->
                let status =
                    update_status_tuple status (p, r, Configured)
                in
                match write_status status with
                    | true -> print_ok (); Some status
                    | false -> print_failed (); None
    in

    match
        configure_check_target_filter status
        |> configure_mark_change_filter name
    with
        | (Some status, Some spkg) ->
            execute_packaging_script_filter spkg
                Tpm_config.configuresh_name (Some status)
            |> configure_confirm_change_filter name
        | _ -> None

let configure_package_if_possible status name =
    match select_status_tuple_by_name status name with
        | None -> print_endline ("Configure_if_possible: Unknown package \"" ^
            name ^ "\""); None
        | Some (p,r,s) when s = Installed ->
            (match List.exists
                (fun n -> not (is_pkg_name_installed status n))
                p.rdeps
            with
                | true -> Some status
                | false -> match !target_system with
                    | "/" -> configure_package status name
                    | _ -> Some status)
        | _ -> Some status

let configure_all_packages_filter status =
    match status with None -> None | Some status ->
    List.fold_left
        (fun status (p,r,ps) ->
            match status with None -> None | Some status ->
            match p.n with
                | None -> Some status
                | Some n -> configure_package_if_possible status n)
        (Some status)
        (select_status_tuple_by_predicate
            (fun (_,_,s) -> s = Installed)
            status)

let configure_all_packages status =
    configure_all_packages_filter (Some status)
        

let remove_package status name =
    match select_status_tuple_by_name status name with
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
    let status = update_status_tuple status (pkg, preason, Removal)
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
        let status = delete_status_tuple status (pkg, preason, Removal)
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
        select_status_tuple_by_name status name
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

