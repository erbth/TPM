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

(* let find_package_version_in_all_repos
    (name : string) (version : version) (arch : arch) =
    let rsps = find_package_in_all_repos name arch
    in
    List.fold_left
        (fun rsp (r, sp) ->
            match rsp with Some rsp -> Some rsp | None ->
            if sp.sv = version then Some (r, sp) else None
        )
        None
        rsps *)


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

(* let find_and_select_packages_in_all_repos (cfg:configuration) names =
    List.fold_left
            (fun a name -> match a with None -> None | Some a ->
                match find_and_select_package_in_all_repos name with
                    | None -> print_endline ("Package \"" ^ name ^
                        "\" not found for architecture " ^ (string_of_arch cfg.a));
                        None
                    | Some rp -> Some (rp::a))
            (Some [])
            (List.rev names) *)