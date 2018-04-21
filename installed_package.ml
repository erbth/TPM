open Util
open Pkg
open Status

let list_installed_packages () =
    print_target ();
    match read_status () with None -> false | Some status ->
    List.iter
        (fun (pkg,_,_) -> print_endline (string_of_pkg pkg))
        (select_all_status_tuples status |> List.sort compare_status_tuples);
    true

let check_installation status print =
    let _,pr =
        (* Accumulator: (double counter, problem) *)
        List.fold_left
            (fun (cts,pr) t ->
                let (pkg, preason, pstate) = t
                in
                let pr =
                    (if pstate <> Configured
                    then
                        if pstate <> Installed
                        then
                            ((if print
                            then
                                print_endline ("\"" ^ (string_of_pkg pkg) ^
                                    "\" is in the following critical dirty " ^
                                    "state: " ^
                                    (string_of_installation_status pstate))
                            else
                                ());
                            check_critical pr true)
                        else
                            ((if print
                            then
                                print_endline ("\"" ^ (string_of_pkg pkg) ^
                                    "\" is in the following noncritical " ^
                                    "dirty state: " ^
                                    (string_of_installation_status pstate))
                            else
                                ());
                            check_non_critical pr true)
                    else pr)
                in
                let pr =
                    (if (List.exists (fun e -> compare_status_tuples e t = 0) cts)
                    then
                        ((if print
                        then
                            print_endline ("Duplicate package: \"" ^
                                (string_of_pkg pkg) ^ "\"")
                        else ());
                        true)
                    else
                        false)
                    |> check_critical pr
                in
                let pr =
                    (List.exists
                        (fun (n, _) ->
                            if (not (is_pkg_name_installed status n))
                            then ((if print
                                then print_endline ("Package \"" ^ n ^
                                    "\" not installed but required by \"" ^
                                    string_of_pkg pkg ^ "\"")
                                else ());
                                true)
                            else (if not (is_pkg_name_configured status n)
                                then ((if print
                                    then print_endline ("Package \"" ^ n ^
                                        "\" not configured but required by \"" ^
                                        string_of_pkg pkg ^ "\"")
                                    else ());
                                    true)
                                else false))
                        pkg.deps)
                    |> check_non_critical pr
                in
                (t::cts, pr))
            ([],No_problem)
            (select_all_status_tuples status)
    in
    pr

let show_problems_with_installation () =
    print_target ();
    match read_status () with None -> false | Some status ->
    match check_installation status true with
        | No_problem -> print_endline "No problems detected"; true
        | Non_critical -> print_endline
            "Noncritical problems but no critical problems found"; false
        | Critical -> print_endline "Critical problems found"; false

let mark_package
    (name : string)
    (status : status option)
    (adverb : string)
    (reason : installation_reason) =

    match status with None -> None | Some status ->
    print_string_flush ("Marking package \"" ^ name ^
        "\" as " ^ adverb ^ " installed");
    match select_status_tuple_by_name status name with
        | None ->
            print_newline ();
            print_string ("    Package not found in status");
            print_failed ();
            None
        | Some (pkg, ir, state) ->
            let status =
                update_status_tuple status (pkg, reason, state)
            in
            match write_status status with
                | false -> print_failed (); None
                | true -> print_ok (); Some status

let mark_package_manual name status =
    mark_package name status "manually" Manual

let mark_package_auto name status =
    mark_package name status "automatically" Auto

let ui_show_version name =
    match read_status () with
        | None -> false
        | Some status ->

    match select_status_tuple_by_name status name with
        | None -> print_endline "---"; true
        | Some (pkg, _, _) ->
            match (pkg.v) with
                | None ->
                    print_endline "Invalid package (it has no version)";
                    false
                | Some v ->
                    print_endline (string_of_version v);
                    true
