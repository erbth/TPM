open Util
open Pkg
open Repository
open Status
open Packed_package

let install_package status repo pkg reason =
    print_endline ("Installing \"" ^ (string_of_pkg pkg) ^ "\"=" ^
        (match pkg.v with None -> "?" | Some v -> string_of_version v) ^
        " from " ^ (string_of_repository repo));

    let status = unique_insert_status_tupel status (pkg, reason, Installation)
    in
    print_string "    marking the change in status";
    if write_status status |> not then (print_failed (); false)
    else
    (print_ok ();
    
    print_string "    installing files";
    if install_files repo pkg |> not
    then
        (print_failed ();
        print_string "    removing the change from status";
        let status = delete_status_tupel status (pkg, (), ())
        in
        if write_status status then (print_ok (); false)
        else (print_failed (); false))
    else
    (print_ok ();
    
    print_string "    acknowledging the change in status";
    let status = update_status_tupel status (pkg, reason, Installed)
    in
    if write_status status then (print_ok (); true)
    else (print_failed (); false)
    )
    )

let upgrade_package status repo pkg =
    print_endline ("Upgrade: not supported yet (package \"" ^
        (string_of_pkg pkg) ^ "\""); false

let install_or_upgrade_package repo pkg reason =
    match read_status () with None -> false | Some s ->
    match (pkg.t, pkg.n, pkg.v, pkg.a) with
        | (Some t, Some n, Some v, Some a) ->
            (match get_package_status_opt s n v a with
                | None -> install_package s repo pkg reason
                | Some (ipkg,_,_) ->
                    if ipkg.v <> Some v then upgrade_package s repo pkg
                    else (print_endline ("\"" ^ n ^ "\"=" ^ (string_of_version v) ^
                        " is already installed."); true))
        | _ ->
            print_endline "Installation: Invalid package"; false