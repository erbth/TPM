open Util
open Pkg
open Repository
open Repository_search
open Status
open Packed_package
open Installed_package
open Configuration

(* Filters that are useful for constructing pipelines of operations to
 * install and remove packages *)
let fetch_package (name : string) (version : version) (arch : arch)
    (repo : repository) (status : status option) =
    match status with None -> (None, None) | Some status ->
    print_string_flush "    Fetching the package";
    match create_or_clean_tmp_dir () with
        | false -> print_failed (); (None, None)
        | true ->
    match read_package repo name version arch with
        | None -> print_failed (); (None, None)
        | Some pkg ->
    match static_of_dynamic_pkg pkg with
        | None -> print_newline (); print_string
            "    Information is missing in the package";
            print_failed (); (None, None)
        | Some spkg ->
    match unpack_packed_shape repo name version arch with
        | false -> print_failed (); (None, None)
        | true -> print_ok (); (Some status, Some spkg)

let check_for_file_collisions
    (msg : string)
    (files : string list)
    (status : status option) =
    match status with None -> None | Some status ->
    print_string_flush msg;
    match
        List.fold_left
            (fun s fn ->
                match file_status (form_target_path fn) with
                    | Directory
                    | Other_file -> print_newline (); print_string_flush
                        ("    \"" ^ fn ^ "\" exists already"); None
                    | Read_error -> print_newline (); print_string_flush
                        ("    can not check for \"" ^ fn ^ "\""); None
                    | Non_existent -> s)
            (Some status)
            files
    with
        | None -> print_failed (); None
        | Some status -> print_ok (); Some status

let unpack_files_to_filter spkg destination (excluded_files, status) =
    match status with None -> None | Some status ->
    (* This flushes stdout *)
    print_string_flush "    Unpacking files";
    match unpack_files_to spkg destination excluded_files status with
        | false -> print_failed (); None
        | true -> print_ok (); Some status

let create_pkg_info_location (spkg : static_pkg) (status : status option) =
    match status with None -> None | Some status ->
    print_string_flush "    Creating the package info location";
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

let remove_pkg_info_location
    (spkg : static_pkg)
    (status : status option) =

    match status with None -> None | Some status ->
    print_string_flush "    Removing the package info location";
    let path =
        form_target_path (Tpm_config.package_info_location ^ "/" ^ spkg.sn)
    in
    if
        match file_status path with
            | Non_existent -> true
            | Directory -> rmdir_r path
            | Read_error ->
                print_newline ();
                print_string "    Read error";
                false
            | Other_file ->
                print_newline ();
                print_string ("    \"" ^ path ^ "\" is not a directory");
                false
    then
        (print_ok (); Some status)
    else
        (print_failed (); None)

let copy_packaging_scripts (spkg : static_pkg) (status : status option) =
    match status with None -> None | Some status ->
    print_string_flush "    Copying files to the package info location";
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
                    then s::c
                    else c)
                []
                all_packaging_scripts)
        with
            | Sys_error msg ->
                print_newline ();
                print_string_flush msg;
                print_failed ();
                None
            | _ -> None
    with
        | None -> print_failed (); None
        | Some pkg_scripts ->
    try
        install_files (0, 0) 0o755 pkg_scripts package_info_location;
        print_ok ();
        Some status
    with
        | Unix.Unix_error (c, _, _) ->
            print_newline ();
            print_string ("    " ^ Unix.error_message c);
            print_failed (); None
        | _ -> print_failed (); None

let determine_files_to_exclude (spkg : static_pkg) (status : status option) =
    let conf_determine_files_to_exclude spkg status =
            ([], status)
    in
    let sw_determine_files_to_exclude spkg status =
        match status with None -> [], None | Some status ->
        print_string_flush "    Determining wich config files must be excluded";
        match
            List.fold_left
                (fun (status, efs) (csum, cf) ->
                    match status with None -> (None, []) | Some status ->
                    let target_path =
                        form_target_path cf
                    in
                    match file_status (target_path) with
                        | Other_file ->
                            (match sha512sum_of_file_opt target_path with
                                | None -> (None, [])
                                | Some is_sum ->
                            match is_sum = csum with
                                | true -> (Some status, efs)
                                | false ->
                            print_newline ();
                            print_endline
                                ("        config file \"" ^ cf ^ "\" does " ^
                                "already exist and differs");
                            print_string_flush
                                ("        from the package's version, hence " ^
                                "not installing it");
                            (Some status, cf::efs))
                        | Non_existent -> (Some status, efs)
                        | Directory -> print_newline (); print_string_flush
                            ("    config file \"" ^ cf ^
                            "\" does already exist as directory");
                            (None, [])
                        | Read_error -> print_newline (); print_string_flush
                            ("    read error while testing for config file \"" ^
                            cf ^ "\"");
                            (None, []))
                (Some status, [])
                (List.rev spkg.scfiles)
        with
            | (None, _) -> print_failed (); ([], None)
            | (Some status, l) -> print_ok (); (l, Some status)
    in
    match status with None -> [], None | Some status ->
    (match spkg.st with
        | Conf -> conf_determine_files_to_exclude spkg (Some status)
        | Sw -> sw_determine_files_to_exclude spkg (Some status))

let remove_fs_items
    ((files : string list), (cfiles : (string * string) list),
        (dirs : string list))
    (force : bool)
    (status : status option) =

    let determine_files (status : status option) =
        match status with None -> None | Some status ->
        print_string_flush "    Determining wich files must be removed";
        let rmfiles =
            List.fold_left
                (fun fs (cks, n) ->
                    match fs with None -> None | Some fs ->
                    match sha512sum_of_file_opt (form_target_path n) with
                        | None -> print_newline (); print_string_flush
                            ("    failed to calculate sha512sum of file \"" ^
                            n ^ "\"");
                            if force then Some (n::fs) else None
                        | Some rs -> if rs = cks then Some (n::fs)
                            else
                            (print_newline ();
                            print_string_flush ("    not removing config " ^
                                "file \"" ^ n ^ "\" because it was modified");
                            Some fs))
                (* The config files shall be removed last. Hence reverse the
                 * list. *)
                (Some (List.rev files))
                cfiles
        in
        match rmfiles with None -> print_failed (); None | Some rmfiles ->
        (* Reverse the list a second time *)
        let rmfiles = List.rev rmfiles
        in
        print_ok ();
        Some rmfiles
    in

    let remove_file n =
        try
            Sys.remove n;
            true
        with
            | Sys_error msg -> print_newline ();
                print_string_flush ("    failed to remove \"" ^
                n ^ "\": " ^ msg); false
            | _ -> print_newline ();
                print_string_flush ("    failed to remove \"" ^ n ^
                "\""); false
    in

    let remove_directory n =
        try
            Unix.rmdir n;
            true
        with
            | Unix.Unix_error (c,_,_) -> print_newline(); print_string_flush
                ("    failed to remove directory \"" ^ n ^ "\": " ^
                (Unix.error_message c)); false
            | _ -> print_newline (); print_string_flush
                ("    failed to remove directory \"" ^ n ^ "\""); false
    in

    let remove_files (files : string list) (status : status option) =
        match status with None -> None | Some status ->
        print_string_flush "    Removing files";
        let s =
            List.fold_left
                (fun s fn ->
                    let fn = form_target_path fn
                    in
                    match file_status fn with
                        | Other_file -> remove_file fn && s
                        | Non_existent -> (print_newline (); print_string_flush
                            ("    \"" ^ fn ^
                            "\" does not exist, hence not removing it"); s)
                        | Read_error -> print_newline (); print_string_flush
                            ("    cannot read \"" ^ fn ^ "\""); false
                        | Directory -> print_newline (); print_string_flush
                            ("    \"" ^ fn ^ "\" is a directory"); false)
                true
                files
        in
        if not s then (print_failed (); None)
        else (print_ok (); Some status)
    in

    let remove_directories (dirs : string list) (status : status option) =
        match status with None -> None | Some status ->
        print_string_flush "    Removing directories";
        let s =
            List.fold_left
                (fun s dn ->
                    let dn = form_target_path dn
                    in
                    match file_status dn with
                        | Directory ->
                            (try
                                if Sys.readdir dn |> array_is_empty
                                then remove_directory dn && s
                                else s
                            with
                                | Sys_error msg -> print_newline ();
                                    print_string msg; false
                                | _ -> print_newline ();
                                    print_string_flush ("    can not read " ^
                                        "from \"" ^ dn ^ "\"");
                                    false)
                        | Other_file -> print_newline (); print_string_flush
                            ("    \"" ^ dn ^ "\" is not a directory"); false
                        | Non_existent -> print_newline (); print_string_flush
                            ("    \"" ^ dn ^ "\" does not exist hence not " ^
                            "removing it");
                            s
                        | Read_error -> print_newline (); print_string_flush
                            ("    cannot read from \"" ^ dn ^ "\""); false)
                true
                (* The directory list is sorted therefore processing it in reverse
                * order means processing from inner to outer. *)
                (List.rev dirs)
        in
        if not s then (print_failed (); None)
        else (print_ok (); Some status)
    in
    match determine_files status with
        | None -> None
        | Some rmfiles ->
    remove_files rmfiles status
    |> remove_directories dirs

let execute_packaging_script_if_exists
    (pkg_name : string)
    (script_name : string)
    (status : status option) =

    match status with None -> None | Some status ->
    print_string_flush ("    Checking for " ^ script_name);    
    let spath =
        form_target_path (
            Tpm_config.package_info_location ^ "/" ^
            pkg_name ^ "/" ^
            script_name)
    in
    match file_status spath with
        | Non_existent ->
            print_ok ();
            Some status
        | Directory ->
            print_newline ();
            print_string ("    It is a directory");
            print_failed ();
            None
        | Read_error ->
            print_newline ();
            print_string ("    Read error");
            print_failed ();
            None
        | Other_file ->
    print_ok ();
    print_string_flush ("    Running " ^ script_name);
    let spath_args =
        [| spath |]
    in
    try
        Unix.chdir (form_target_path "/");
        match run_program spath_args with
            | (_, WEXITED 0) ->
                print_ok (); Some status
            | (_, WEXITED c) ->
                print_newline ();
                print_string ("    " ^ script_name ^ " failed with code " ^
                    string_of_int c);
                print_failed ();
                None
            | _ ->
                print_newline ();
                print_string ("    " ^ script_name ^ " failed");
                print_failed ();
                None
    with
        Unix.Unix_error (c, _, _) ->
            print_newline ();
            print_string
                ("    Executing  " ^ script_name ^ " failed: " ^
                Unix.error_message c);
            print_failed ();
            None

let unconfigure_package (name : string) (status : status option) =
    match status with None -> None | Some status ->
    print_string_flush "    Checking runtime for unconfiguring";
    match !runtime_system with
        | Directory_runtime _ ->
            print_newline ();
            print_string
                ("    A package can only be unconfigured on a native " ^
                "runtime system");
            print_failed ();
            None
        | Native_runtime ->
    print_ok ();
    execute_packaging_script_if_exists
        name
        Tpm_config.unconfiguresh_name
        (Some status)

(* Install a package from a specific repository and mark it with a specific
 * reason for installing it. *)
let elementary_install_package cfg name version reason repo status =
    match status with None -> None | Some status ->
    print_endline ("Installing package \"" ^ name ^ "\"=" ^
        string_of_version version ^ " from " ^ (string_of_repository repo));

    let check_not_installed (name : string) (status : status option) =
        match status with None -> None | Some status ->
        match select_status_tuple_by_name status name with
            | None -> Some status
            | Some _ ->
                print_endline "    The package is already registered in the database";
                None
    in

    let mark_change (spkg : static_pkg) (reason : installation_reason)
        ((excluded_files : string list), (status : status option)) =
        match status with None -> [], None | Some status ->
        print_string_flush "    Marking the change in status";
        let status =
            unique_insert_status_tuple status
                (dynamic_of_static_pkg spkg, reason, Installing)
        in
        if write_status status |> not then (print_failed (); [], None)
        else (print_ok (); (excluded_files, Some status))
    in

    let confirm_change (name : string) (status : status option) =
        match status with None -> None | Some status ->
        print_string "    Commiting changes to status";
        match select_status_tuple_by_name status name with
            | None -> print_newline (); print_string
                "    the package vanished from status";
                print_failed (); None
            | Some (pkg, reason, _) ->
                let status = update_status_tuple status (pkg, reason, Installed)
                in
                if write_status status
                then (print_ok (); Some status)
                else (print_failed (); None)
    in
    
    let status = check_not_installed name (Some status)
    in
    match fetch_package name version cfg.a repo status with
        | (None, _)
        | (Some _, None) -> None
        | (Some status, Some spkg) ->
    check_for_file_collisions
        "    Checking if any of the package's files exist already"
        spkg.sfiles
        (Some status)
    |> determine_files_to_exclude spkg
    |> mark_change spkg reason
    |> unpack_files_to_filter spkg (
        match !runtime_system with
            | Native_runtime -> "/"
            | Directory_runtime d -> d
    )
    |> create_pkg_info_location spkg
    |> copy_packaging_scripts spkg
    |> confirm_change name

(* Change a package from a specific repository and mark it with a specific
 * reason for installing it. This is the elementary procedure that shall be
 * used when a package is upgrades, downgraded or reinstelled. *)
let elementary_change_package cfg name version reason repo status =
    match status with None -> None | Some status ->
    print_endline ("Changing package \"" ^ name ^ "\" to version " ^
        string_of_version version ^ " from " ^ (string_of_repository repo));

    let check_current (name : string) (status : status option) =
        match status with None -> None | Some status ->
        match select_status_tuple_by_name status name with
            | None ->
                print_endline "    The package is not installed";
                None
            | Some (old_pkg, _, old_state) ->
                match static_of_dynamic_pkg old_pkg with
                    | None ->
                        print_endline "    The current package is invalid";
                        None
                    | Some old_spkg ->
                        print_endline ("    Current version is " ^
                            string_of_version old_spkg.sv);
                        match old_state with
                            | Configured
                            | Configuring
                            | Changing_unconf
                            | Changing
                            | Installed ->
                                Some (status, old_spkg, old_state)
                            | Installing
                            | Removing_unconf
                            | Removing ->
                                print_endline
                                    ("    No change is possible from the " ^
                                    "package's current state: " ^
                                    string_of_installation_status old_state);
                                None
    in

    let split_files
        (old_spkg : static_pkg)
        (spkg : static_pkg)
        (status : status option) =

        match status with None -> None | Some status ->
        print_string_flush "    Determining which files were added or removed";
        let (added_files, files_to_remove) =
            sorted_bidirectional_difference
                compare_names
                spkg.sfiles
                old_spkg.sfiles
        in
        let (added_cfiles, cfiles_to_remove) =
            sorted_bidirectional_difference
                compare_cfile_pair
                spkg.scfiles
                old_spkg.scfiles
        in
        let (added_dirs, dirs_to_remove) =
            sorted_bidirectional_difference
                compare_names
                spkg.sdirs
                old_spkg.sdirs
        in
        print_ok ();
        Some (
            (added_files, added_cfiles, added_dirs),
            (files_to_remove, cfiles_to_remove, dirs_to_remove),
            status)
    in

    let change_unconfigure_package
        (name : string)
        (pkg_state : installation_status)
        (status : status option) =

        let mark_unconfigure (name : string) (status : status option) =
            match status with None -> None | Some status ->
            print_string_flush "    Marking unconfiguration in status";
            match select_status_tuple_by_name status name with
                | None ->
                    print_newline ();
                    print_string "    The package disappeared from status";
                    None
                | Some (pkg, reason, _) ->
                    let status =
                        update_status_tuple status
                            (pkg, reason, Changing_unconf)
                    in
                    match write_status status with
                        | false -> print_failed (); None
                        | true -> print_ok (); Some status
        in

        match status with None -> None | Some status ->
        match pkg_state with
            | Installing
            | Installed
            | Configuring
            | Changing
            | Removing_unconf
            | Removing -> Some status
            | Configured
            | Changing_unconf ->

        (* Check if the runtime system is native before changing the
         * system's state *)
        match !runtime_system with
            | Directory_runtime _ ->
                print_string
                    ("    The package needs to be unconfigured but the " ^
                    "runtime system is not native.");
                print_failed ();
                None
            | Native_runtime ->

        mark_unconfigure name (Some status)
        |> unconfigure_package name
    in

    let mark_change
        (trans_spkg : static_pkg)
        (reason : installation_reason)
        (added_files, added_cfiles, added_dirs)
        ((excluded_files : string list), (status : status option)) =

        match status with None -> [], None | Some status ->
        print_string_flush "    Marking the change in status";

        let status =
            update_status_tuple status
                (dynamic_of_static_pkg trans_spkg, reason, Changing)
        in
        if write_status status |> not then (print_failed (); [], None)
        else (print_ok (); (excluded_files, Some status))
    in

    let confirm_change
        (spkg : static_pkg) (status : status option) =
        match status with None -> None | Some status ->
        print_string "    Commiting changes to status";
        let status =
            update_status_tuple status
                (dynamic_of_static_pkg spkg, reason, Installed)
        in
        if write_status status
        then (print_ok (); Some status)
        else (print_failed (); None)
    in
    
    match check_current name (Some status) with
        | None -> None
        | Some (status, old_spkg, old_state) ->
    match fetch_package name version cfg.a repo (Some status) with
        | (None, _)
        | (Some _, None) -> None
        | (Some status, Some spkg) ->
    match split_files old_spkg spkg (Some status) with
        | None -> None
        | Some (added_fs_items, fs_items_to_remove, status) ->
    let (added_files, added_cfiles, added_dirs) =
        added_fs_items
    in
    (* trans_spkg is the new package but with the old fs items extended by
     * the new ones therefore not overwriting cfiles' checksums *)
    let trans_spkg =
        {spkg with
            sfiles = sorted_merge compare_names old_spkg.sfiles added_files;
            scfiles = sorted_merge compare_cfile_pair old_spkg.scfiles added_cfiles;
            sdirs = sorted_merge compare_names old_spkg.sdirs added_dirs}
    in
    (* oldconf_spkg is the new package but with the old checksums for previous
     * conf files *)
    let oldconf_spkg =
        let cfiles =
            List.map
                (fun (c, p) ->
                    match rassoc_opt p old_spkg.scfiles with
                        | None -> (c, p)
                        | Some c -> (c, p))
                spkg.scfiles
        in
        {spkg with scfiles = cfiles}
    in
    change_unconfigure_package spkg.sn old_state (Some status)
    |> check_for_file_collisions
        "    Checking if any of the added files exist already"
        added_files
    |> determine_files_to_exclude oldconf_spkg
    |> mark_change trans_spkg reason added_fs_items
    |> unpack_files_to_filter spkg (
        match !runtime_system with
            | Native_runtime -> "/"
            | Directory_runtime d -> d
    )
    |> remove_pkg_info_location spkg
    |> create_pkg_info_location spkg
    |> copy_packaging_scripts spkg
    |> remove_fs_items fs_items_to_remove false
    |> confirm_change spkg

let elementary_configure_package (name : string) (status : status option) =
    match status with None -> None | Some status ->
    print_endline ("Configuring package \"" ^ name ^ "\"");

    let check (name : string) (status : status option) =
        match status with None -> None | Some status ->
        print_string_flush "    Checking for the package and the runtime";
        match !runtime_system with
            | Directory_runtime _ ->
                print_newline ();
                print_string
                    ("    Configuring is only supported on a native runtime " ^
                    "system");
                print_failed ();
                None
            | Native_runtime ->
        match select_status_tuple_by_name status name with
            | None ->
                print_newline ();
                print_string "    Package not found";
                print_failed ();
                None
            | Some (pkg, reason, pstate) ->
                match pstate with
                    | Installing
                    | Configured
                    | Changing
                    | Changing_unconf
                    | Removing
                    | Removing_unconf ->
                        print_newline ();
                        print_string
                            ("    Package in invalid state \"" ^
                            string_of_installation_status pstate ^ "\"");
                        print_failed ();
                        None
                    | Installed
                    | Configuring ->
                        print_ok ();
                        Some ((pkg, reason, pstate), status)
    in

    let mark_change
        (pkg : pkg)
        (reason : installation_reason)
        (status : status option) =

        match status with None -> None | Some status ->
        print_string_flush "    Marking change in status";
        let status =
            update_status_tuple status (pkg, reason, Configuring)
        in
        match write_status status with
            | false ->
                print_failed ();
                None
            | true ->
                print_ok ();
                Some status
    in

    let commit_change
        (pkg : pkg)
        (reason : installation_reason)
        (status : status option) =

        match status with None -> None | Some status ->
        print_string_flush "    Commiting change to status";
        let status =
            update_status_tuple status (pkg, reason, Configured)
        in
        match write_status status with
            | false ->
                print_failed ();
                None
            | true ->
                print_ok ();
                Some status
    in

    match check name (Some status) with
        | None -> None
        | Some ((pkg, reason, pstate), status) ->
    mark_change pkg reason (Some status)
    |> execute_packaging_script_if_exists name Tpm_config.configuresh_name
    |> commit_change pkg reason        

let elementary_remove_package
    (name : string)
    (force : bool)
    (status : status option) =

    match status with None -> None | Some status ->
    print_endline ("Removing package \"" ^ name ^ "\":");
    
    let retrieve_package (name : string) (status : status option) =
        match status with None -> None | Some status ->
        print_string_flush "    Retrieving the package from status";
        match select_status_tuple_by_name status name with
            | None ->
                print_failed ();
                None
            | Some (pkg, reason, pstate) ->
                match static_of_dynamic_pkg pkg with
                    | None ->
                        print_newline ();
                        print_string "    Invalid package in status";
                        print_failed ();
                        None
                    | Some spkg ->
                        print_ok ();
                        Some (spkg, reason, pstate, status)
    in

    let remove_unconfigure_package
        (name : string)
        (pkg_state : installation_status)
        (status : status option) =

        let mark_unconfigure (name : string) (status : status option) =
            match status with None -> None | Some status ->
            print_string_flush "    Marking unconfiguration in status";
            match select_status_tuple_by_name status name with
                | None ->
                    print_newline ();
                    print_string "    The package disappeared from status";
                    print_failed ();
                    None
                | Some (pkg, reason, _) ->
                    let status =
                        update_status_tuple status
                            (pkg, reason, Removing_unconf)
                    in
                    match write_status status with
                        | false -> print_failed (); None
                        | true -> print_ok (); Some status
        in

        match status with None -> None | Some status ->
        match pkg_state with
            | Installing
            | Installed
            | Changing
            | Removing -> Some status
            | Configured
            | Configuring
            | Changing_unconf
            | Removing_unconf ->

        (* Check if the runtime system is native before changing the
         * system's state *)
        match !runtime_system with
            | Directory_runtime _ ->
                print_string
                    ("    The package needs to be unconfigured but the " ^
                    "runtime system is not native.");
                print_failed ();
                None
            | Native_runtime ->

        mark_unconfigure name (Some status)
        |> unconfigure_package name
    in

    let mark_removal
        (spkg : static_pkg)
        (reason : installation_reason)
        (status : status option) =

        match status with None -> None | Some status ->
        print_string_flush "    Marking removal in status";
        let status =
            update_status_tuple status
                (dynamic_of_static_pkg spkg, reason, Removing)
        in
        match write_status status with
            | false -> print_failed (); None
            | true -> print_ok (); Some status
    in

    let commit_removal
        (spkg : static_pkg)
        (reason : installation_reason)
        (status : status option) =

        match status with None -> None | Some status ->
        print_string_flush "    Removing package from status";
        let status =
            delete_status_tuple status
                (dynamic_of_static_pkg spkg, reason, Removing)
        in
        match write_status status with
            | false -> print_failed (); None
            | true -> print_ok (); Some status
    in

    match retrieve_package name (Some status) with
        | None -> None
        | Some (spkg, reason, pstate, status) ->
    let fs_items_to_remove =
        (spkg.sfiles, spkg.scfiles, spkg.sdirs)
    in
    remove_unconfigure_package name pstate (Some status)
    |> mark_removal spkg reason
    |> remove_fs_items fs_items_to_remove force
    |> remove_pkg_info_location spkg
    |> commit_removal spkg reason
