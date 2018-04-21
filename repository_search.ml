open Pkg
open Util
open Configuration
open Repository
open Packed_package

let find_package_dir_repo p name =
    let find_in_arch a name acc =
        let arch_dir = p ^ "/" ^ (string_of_arch a)
        in
        try
            List.fold_left
                (fun acc fn ->
                    match pkg_split_packed_name fn with
                        | None -> acc
                        | Some (pn, pv) ->
                    if name = pn
                    then (match
                            Packed_package.read_package
                                (DirectoryRepository p)
                                pn
                                pv
                                a
                        with
                            | Some pkg -> pkg::acc
                            | None -> print_endline
                                ("Repo: Could not read package file \"" ^
                                fn ^ "\" from directory repository \"" ^ p ^
                                "\" @ " ^ (string_of_arch a) ^ "."); acc)
                    else acc)
                acc
                (Sys.readdir arch_dir |> Array.to_list)
        with
            | Sys_error msg -> print_endline
                ("Repo: Could not read architecture " ^ (string_of_arch a) ^
                " of directory repository \"" ^ p ^ "\": " ^ msg);
                acc
            | _ -> print_endline
                ("Repo: Could not read architecture " ^ (string_of_arch a) ^
                " of directory repository \"" ^ p ^ "\"");
                acc
    in
    let archs =
        try
            List.fold_left
                (fun acc d -> if Sys.is_directory (p ^ "/" ^ d)
                    then match arch_of_string d with
                        | Some a -> a::acc
                        | None -> acc
                    else acc)
                []
                (Sys.readdir p |> Array.to_list)
        with
            | Sys_error msg -> print_endline
                ("Repository: Could not read from directory repository \"" ^
                p ^ "\": " ^ msg); []
            | _ -> print_endline
                ("Repository: Could not read from directory repository \"" ^
                p ^ "\""); []
    in
    List.fold_left (fun acc a -> find_in_arch a name acc) [] archs

let find_package repo = match repo with
    | DirectoryRepository p -> find_package_dir_repo p

let find_package_in_all_repos (name : string) =
    match read_configuration () with None -> [] | Some cfg ->
    List.fold_left
        (fun rsps r ->
            let pkgs = find_package r name
            in
            List.fold_left
                (fun rsps p ->
                    match static_of_dynamic_pkg p with
                        | Some sp -> (r, sp)::rsps
                        | None -> rsps
                )
                rsps
                pkgs
        )
        []
        cfg.repos

let select_version_to_install
    (cs : package_constraint list)
    (arch : arch)
    (rps : (repository * static_pkg) list) =

    List.fold_left
        (fun a (r,sp) ->
            match cs_satisfied sp.sv cs with
                | false -> a
                | true ->
            match sp.sa = arch with
                | false -> a
                | true ->
            match a with None -> Some (r,sp) | Some (ar, asp) ->
            if spkg_newer sp asp then Some (r, sp) else Some (ar, asp))
        None
        rps

let find_and_select_package_in_all_repos
    (name : string)
    (cs : package_constraint list)
    (arch : arch) =

    find_package_in_all_repos name |> select_version_to_install cs arch

(* This returns a list of all available versions of the package that satisfy
 * the given constraints. The list is sorted descending by version *)
let find_and_filter_package_in_all_repos
    (name : string)
    (cs : package_constraint list)
    (arch : arch) =

    let rps =
        find_package_in_all_repos name
    in
    List.filter
        (fun (r, sp) -> cs_satisfied sp.sv cs && sp.sa = arch)
        rps
    |> List.sort
        (fun (_, sp1) (_, sp2) -> compare_version sp2.sv sp1.sv)
