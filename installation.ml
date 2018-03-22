open Util
open Pkg
open Repository
open Repository_search
open Status
open Packed_package
open Installed_package
open Configuration

let install_package status repo pkg reason =
    print_endline ("Installing \"" ^ (string_of_pkg pkg) ^ "\"=" ^
        (match pkg.v with None -> "?" | Some v -> string_of_version v) ^
        " from " ^ (string_of_repository repo));

    let status = unique_insert_status_tupel status (pkg, reason, Installation)
    in
    print_string "    Marking the change in status";
    if write_status status |> not then (print_failed (); None)
    else
    (print_ok ();

    print_string "    Determining wich config files must be excluded";
    let files_to_exclude =
        List.fold_left
            (fun efs (_,cf) ->
                match efs with None -> None | Some efs ->
                match file_status (form_target_path cf) with
                    | Other_file -> print_newline (); print_string
                        ("    config file \"" ^ cf ^ "\" does already exist, "
                        ^ "hence not installing it");
                        Some (cf::efs)
                    | Non_existent -> Some efs
                    | Directory -> print_newline (); print_string
                        ("    config file \"" ^ cf ^ "\" does already exist " ^
                        "as directory");
                        None
                    | Read_error -> print_newline (); print_string
                        ("    read error while testing for config file \"" ^
                        cf ^ "\"");
                        None)
            (Some [])
            (pkg.cfiles)
    in
    match files_to_exclude with
        | None -> print_failed (); None
        | Some files_to_exclude -> print_ok ();    
    
    print_string "    installing files";
    if install_files repo pkg files_to_exclude |> not
    then
        (print_failed ();
        print_string "    Removing the change from status";
        let status = delete_status_tupel status (pkg, (), ())
        in
        if write_status status then (print_ok (); None)
        else (print_failed (); None))
    else
    (print_ok ();
    
    print_string "    Acknowledging the change in status";
    let status = update_status_tupel status (pkg, reason, Installed)
    in
    if write_status status then (print_ok (); Some status)
    else (print_failed (); None)
    )
    )

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

    print_string "    Removing files";
    let remove_file n =
        try
            Sys.remove n;
            true
        with
            | Sys_error msg -> print_newline ();
                print_string ("    failed to remove \"" ^
                n ^ "\" :" ^ msg); false
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
                        if Sys.readdir dn |> array_is_empty
                        then
                            remove_directory dn && s
                        else
                            (print_newline ();
                            print_string ("    \"" ^ dn ^
                                "\" is not empty hence not removing it"); s)
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

    print_string "    Removing package from status";
    let status = delete_status_tupel status (pkg, preason, Removal)
    in
    if write_status status |> not then (print_failed (); None)
    else (print_ok (); Some status)
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

