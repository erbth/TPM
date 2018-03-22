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

let check_installation status print =
    let good,_ =
        List.fold_left
            (fun (g,cts) t ->
                let (pkg, preason, pstate) = t
                in
                let g =
                if pstate <> Installed
                then
                    (print_endline ("\"" ^ (string_of_pkg pkg) ^
                        "\" is in the following dirty state: " ^
                        (string_of_installation_status pstate));
                    false)
                else g
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
                (g, t::cts))
            (true,[])
            status
    in
    (if print
    then
        if good
        then print_endline "No problems detected"
        else print_endline "Problems found"
    else
        ());
    good

let show_problems_with_installation () =
    print_target ();
    match read_status () with None -> false | Some status ->
    check_installation status true