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
            if
                rmdir_r (
                    form_target_path
                        (Tpm_config.package_info_location ^ "/" ^ spkg.sn))
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
                (spkg.scfiles)
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
                            n ^ "\""); None
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

let execute_packaging_script (pkg_name : string) (script_name : string) =
    let spath_args =
        [| form_target_path (
            Tpm_config.package_info_location ^ "/" ^
            pkg_name ^ "/" ^
            script_name) |]
    in
    try
        match run_program spath_args with
            | (_, WEXITED 0) -> ()
            | _ -> raise (Gp_exception (script_name ^ " failed"))
    with
        Unix.Unix_error (c, _, _) ->
            raise (Gp_exception ("Executing  " ^ script_name ^
            " failed: " ^ Unix.error_message c))

let unconfigure_package (name : string) (status : status option) =
    match status with None -> None | Some status ->
    print_string_flush "    Unconfiguring the package";
    match !runtime_system with
        | Directory_runtime _ ->
            print_newline ();
            print_string
                ("    A package can only be unconfigured on a native " ^
                "runtime system");
            print_failed ();
            None
        | Native_runtime ->

    try
        execute_packaging_script name Tpm_config.unconfiguresh_name;
        print_ok ();
        Some status
    with
        | Gp_exception msg ->
            print_newline ();
            print_string ("    " ^ msg);
            print_failed ();
            None

(* let execute_packaging_script_filter spkg scriptname status =
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
                (!program_cd ^ " " ^ !runtime_system ^ " && " ^ script)
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
            | None -> print_failed (); None) *)

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
    |> remove_fs_items fs_items_to_remove
    |> confirm_change spkg

(* let upgrade_package status repo pkg =
    print_endline ("Upgrade: not supported yet (package \"" ^
        (string_of_pkg pkg) ^ "\""); None

let install_packages
    (status : status)
    (nvrl : (string * version * installation_reason) list) =
    let ig =
        build_dependency_graph nvrl
    in
    raise (Critical_error "Not implemented 4")

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
        match !runtime_system with
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
                | false -> match !runtime_system with
                    | "/" -> configure_package status name
                    | _ -> Some status)
        | _ -> Some status

let configure_packages_if_possible_filter names status =
    List.fold_left
        (fun status n ->
            match status with None -> None | Some status ->
            configure_package_if_possible status n)
        status
        names

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
    true *)
