open Arg
open Util
open Pkg
open Unpacked_package
(* open Packed_package *)
open Installed_package
(* open Repository_search
open Installation *)
open Configuration
open Status
open Depres

let ignore_noncritical = ref false
let ignore_dependencies  = ref false

(* let install_packages_filter ignore_noncritical ignore_deps names status =
    match status with None -> None | Some status ->
    match read_configuration () with
        | None -> None
        | Some cfg ->
    let rps =
        find_and_select_packages_in_all_repos cfg names
    in
    match rps with None -> None | Some rps ->
    match check_installation status true with
        | Critical ->
            print_endline "Critical problems prevent the installation"; None
        | Non_critical when not ignore_noncritical ->
            print_endline "Noncritical problems prevent the installation"; None
        | Non_critical
        | No_problem ->
            let rprs = List.map (fun (r, p) -> (r, p, Manual)) rps
            in
            let remove_configured_pkgs u_pkgs status =
                match status with None -> (None, []) | Some status->
                List.fold_left
                    (fun (status, l) n ->
                        match status with None -> (None, []) | Some status ->
                        if is_pkg_name_configured status n
                        then (Some status, l)
                        else (Some status, n::l))
                    (Some status, [])
                    u_pkgs
            in
            let unconfigured_pkgs =
                select_status_tuple_by_predicate
                    (fun (_,_,s) -> s = Installed)
                    status
                |> List.map (fun (p,_,_) -> unopt p.n)
            in
            try
                let rprs =
                    if not ignore_deps
                    then
                        let g =
                            dependency_graph_of_repo_package_reason_list
                                cfg
                                rprs
                        in
                        derive_installation_order_from_graph g rps
                    else rprs
                in
                List.fold_left
                    (fun (s, unconfigured_pkgs) (r,p,ir) ->
                        match s with None -> (None, []) | Some s ->
                        let unconfigured_pkgs =
                            unconfigured_pkgs @ [unopt p.n]
                        in
                        install_or_upgrade_package s r p ir
                        |> configure_packages_if_possible_filter unconfigured_pkgs
                        |> remove_configured_pkgs unconfigured_pkgs)
                    (Some status, unconfigured_pkgs)
                    rprs
                |> fst
            with
                Gp_exception -> None *)

let install_packages_ui names =
    print_target ();
    match read_configuration () with
        | None -> false
        | Some cfg ->
    match read_status () with
        | None -> false
        | Some status ->
    match
        List.fold_left
            (fun a n ->
                match a with None -> None | Some a ->
                match pkg_name_constraints_of_string n with
                    | None ->
                        print_endline
                            ("Invalid package description \"" ^ n ^ "\"");
                        None
                    | Some (n, cs) ->
                        let cs =
                            List.map (fun c -> (c, None)) cs
                        in
                        Some ((n, cs, Manual) :: a))
            (Some [])
            names
    with
        | None -> false
        | Some ncrl ->
    match build_igraph cfg status ncrl with
        | None -> false
        | Some ig ->
    install_configure_from_igraph cfg status ig
    |> bool_of_option

let mark_manual_ui names =
    List.fold_left
        (fun status name ->
            mark_package_manual name status)
        (read_status ())
        names
    |> bool_of_option

let mark_auto_ui names =
    List.fold_left
        (fun status name ->
            mark_package_auto name status)
        (read_status ())
        names
    |> bool_of_option

(* let remove_packages_filter ignore_noncritical ignore_deps names status =
    match status with None -> None | Some status ->
    match check_installation status true with
        | Critical ->
            print_endline "Critical problems prevent from removing"; None
        | Non_critical when not ignore_noncritical ->
            print_endline "Noncritical problems prevent from removing"; None
        | Non_critical
        | No_problem ->
    let names =
        List.fold_left
            (fun ns n ->
                match select_status_tuple_by_name status n with
                    | None -> print_endline
                        ("Package " ^ n ^ " is not installed");
                        ns
                    | Some _ -> n::ns)
            []
            names
        |> List.rev
    in
    List.fold_left
        (fun s name -> match s with None -> None | Some s ->
            remove_package s name)
        (Some status)
        names

let remove_packages_ui names =
    print_target();
    read_status ()
    |> remove_packages_filter !ignore_noncritical !ignore_dependencies names
    |> bool_of_option

let recover_from_dirty_state () =
    print_target ();
    match read_configuration () with None -> false | Some cfg ->
    match read_status () with None -> false | Some status ->
    let recover_critical_package_state_filter status (pkg, reason, pstate) =
        match status with None -> None | Some status ->
        match pstate with
            | Installed
            | Configured -> Some status
            | _ -> match pkg.n with
                | None -> print_endline "Package with no name, aborting";
                        None
                | Some name -> force_remove status name
    in
    let recover_critical_package_states_filter status =
        match status with None -> None | Some status ->
        List.fold_left
            recover_critical_package_state_filter
            (Some status)
            (select_all_status_tuples status)
    in
    let remove_duplicate_packages_filter status =
        let find_duplicate_packages_filter status =
            match status with None -> (None, [], []) | Some status ->
            ( Accumulator: (status, processed packages, duplicate packages) )
            List.fold_left
                (fun (status, pps, dps) (p,r,s) ->
                    match status with None -> (None, [], []) | Some status ->
                    match
                        List.exists
                            (fun pp -> compare_pkgs_by_name p pp = 0)
                            pps
                    with
                        | true ->
                            (match
                                List.exists
                                    (fun dp -> compare_pkgs_by_name p dp = 0)
                                    dps
                            with
                                | true -> (Some status, pps, p::dps)
                                | false -> (Some status, pps, p::p::dps))
                        | false -> (Some status, p::pps, dps))
                (Some status, [], [])
                (select_all_status_tuples status)
        in
        let (status, _, dps) =
            find_duplicate_packages_filter status
        in
        List.fold_left
            (fun status dp ->
                match status with None -> None | Some status ->
                match dp.n with
                    | None -> print_endline "Package with no name, aborting"; None
                    | Some n -> force_remove status n)
            status
            dps
    in
    let compute_missing_packages_filter status =
        match status with None -> (None, []) | Some status ->
        ( Accumulator: (status, missing package names) )
        List.fold_left
            (fun (status, mps) (p,r,ps) ->
                match status with None -> (None, []) | Some status ->
                List.fold_left
                    (fun (status, mps) n ->
                        match status with None -> (None, []) | Some status ->
                        if not (is_pkg_name_installed status n)
                        then (Some status, n::mps)
                        else (Some status, mps))
                (Some status, mps)
                p.rdeps)
            (Some status, [])
            (select_all_status_tuples status)
    in
    let install_missing_packages_filter (status, mpns) =
        install_packages_filter true false mpns status
    in
    let check_installation_filter = function
        None -> false | Some status ->
        print_newline ();
        print_endline "--- Checking the installation after the recovery ---";
        match check_installation status true with
            | No_problem -> print_endline "Recovery successful"; true
            | _ -> print_endline "Recovery failed"; false
    in
    
    Some status
    |> recover_critical_package_states_filter
    |> remove_duplicate_packages_filter
    |> compute_missing_packages_filter
    |> install_missing_packages_filter
    |> configure_all_packages_filter
    |> check_installation_filter *)


(* User interface *)
let version_msg =
    let (major,minor,revision) = Tpm_config.version
    in
        ("TSClient LEGACY Package Manager version " ^
        (string_of_int major) ^ "." ^
        (string_of_int minor) ^ "." ^
        (string_of_int revision))

let usage_msg = version_msg

(* Read environment variables *)
let read_env_vars () =
    try runtime_system :=
        match
            Unix.getenv "TPM_TARGET"
            |> path_remove_double_slash
        with
            | "" -> !runtime_system
            | "/" -> Native_runtime
            | te -> Directory_runtime te
    with _ -> ();
    try program_sha512sum := Unix.getenv "TPM_PROGRAM_SHA512SUM" with _ -> ();
    try program_tar := Unix.getenv "TPM_PROGRAM_TAR" with _ -> ();
    try program_gzip := Unix.getenv "TPM_PROGRAM_GZIP" with _ -> ()

(* Commands *)
let create_desc_type = ref None
let cmd_create_desc s = create_desc_type := Some s

let print_version = ref None
let cmd_print_version () = print_version := Some ()

let cmd_runtime_system p =
    match path_remove_double_slash p with
        | "" -> ()
        | "/" -> runtime_system := Native_runtime
        | d -> runtime_system := Directory_runtime d

let show_missing = ref None
let cmd_show_missing () = show_missing := Some ()

let set_name = ref None
let cmd_set_name n = set_name := Some n

let set_version = ref None
let cmd_set_version v = set_version := Some v

let set_architecture = ref None
let cmd_set_architecture a = set_architecture := Some a

let add_files = ref None
let cmd_add_files () = add_files := Some ()

let add_dependency = ref None
let cmd_add_dependency s = add_dependency := Some s

let remove_dependencies = ref None
let cmd_remove_dependencies () = remove_dependencies := Some ()

let pack = ref None
let cmd_pack () = pack := Some ()

let install = ref None
let cmd_install () = install := Some ()

let policy = ref None
let cmd_policy n = policy := Some n

let remove = ref None
let cmd_remove () = remove := Some ()

let list_installed = ref None
let cmd_list_installed () = list_installed := Some ()

let show_problems = ref None
let cmd_show_problems () = show_problems := Some ()

let recover = ref None
let cmd_recover () = recover := Some ()

let installation_graph = ref None
let cmd_installation_graph () = installation_graph := Some ()

let reverse_dependencies = ref None
let cmd_reverse_dependencies s = reverse_dependencies := Some s

let op_mark_manual = ref None
let cmd_mark_manual () = op_mark_manual := Some ()

let op_mark_auto = ref None
let cmd_mark_auto () = op_mark_auto := Some ()

let op_show_version = ref None
let cmd_show_version n = op_show_version := Some n

let cmd_specs = [
    ("--version", Unit cmd_print_version, "Print the program's version");
    ("--target", String cmd_runtime_system, "Root of the managed system's filesystem");
    ("--ignore-noncritical", Set ignore_noncritical, "Ignore noncritical problems");
    ("--ignore-dependencies", Set ignore_dependencies, "Do not respect the " ^
        "package's dependencies during installation, removal or upgrade");
    ("--create-desc", String cmd_create_desc,
        "Create desc.xml with package type and destdir in the current working " ^
        "directory");
    ("--show-missing", Unit cmd_show_missing,
        "List missing essential informatin in the package description");
    ("--set-name", String cmd_set_name, "Set the package's name");
    ("--set-version", String cmd_set_version, "Set the package's version");
    ("--set-arch", String cmd_set_architecture, "Set the package's architecture");
    ("--add-files", Unit cmd_add_files, "Add files in destdir");
    ("--add-dependency", String cmd_add_dependency, "Add a dependency");
    ("--remove-dependencies", Unit cmd_remove_dependencies, "Remove all dependencies");
    ("--pack", Unit cmd_pack, "Create the packed/transport form of the package");
    ("--install", Unit cmd_install, "Install or uprade the specified packages");
    ("--policy", String cmd_policy, "Show the installed and available versions of name");
    ("--show-version", String cmd_show_version, "Print a package's version number " ^
        "or `---' if it is not installed");
    ("--remove", Unit cmd_remove, "Remove the specified packages and their " ^
        "config files if they were not modified");
    ("--list-installed", Unit cmd_list_installed, "List all installed packages");
    ("--show-problems", Unit cmd_show_problems, "Show all problems with the current " ^
        "installation (i.e. halfly installed packages after an interruption or " ^
        "missing dependencies)");
    ("--recover", Unit cmd_recover, "Recover from a dirty state by deleting all " ^
        "packages that are in a dirty state (always possible due to atomic " ^
        "write operations to status");
    ("--installation-graph", Unit cmd_installation_graph, "Print the dependency " ^
    "graph in the dot format; If packages are specified, they are added to " ^
    "the graph.");
    ("--reverse-dependencies", String cmd_reverse_dependencies, "List the " ^
        "installed packages that depend on the specified package if it is " ^
        "installed");
    ("--mark-manual", Unit cmd_mark_manual, "Mark the specified packages as " ^
        "manually installed");
    ("--mark-auto", Unit cmd_mark_auto, "Mark the specified packages as " ^
        "automatically installed");
]

let anon_args = ref []
let cmd_anon a =
    if
        !install = Some () ||
        !remove = Some () ||
        !installation_graph = Some () ||
        !op_mark_manual = Some () ||
        !op_mark_auto = Some ()
    then anon_args := (a::(!anon_args |> List.rev)) |> List.rev
    else(print_endline ("Invalid option \"" ^ a ^ "\""); exit 2)

let check_cmdline () =
    let args = [
        PolyUnitOption !print_version;
        PolyStringOption !create_desc_type;
        PolyUnitOption !show_missing;
        PolyStringOption !set_name;
        PolyStringOption !set_version;
        PolyStringOption !set_architecture;
        PolyUnitOption !add_files;
        PolyStringOption !add_dependency;
        PolyUnitOption !remove_dependencies;
        PolyUnitOption !pack;
        PolyUnitOption !install;
        PolyStringOption !policy;
        PolyUnitOption !remove;
        PolyUnitOption !list_installed;
        PolyUnitOption !show_problems;
        PolyUnitOption !recover;
        PolyUnitOption !installation_graph;
        PolyStringOption !reverse_dependencies;
        PolyUnitOption !op_mark_manual;
        PolyUnitOption !op_mark_auto;
        PolyStringOption !op_show_version;
    ]
    in
    match
        List.fold_left (fun a x -> match x with
            | PolyUnitOption Some _ -> a + 1
            | PolyStringOption Some _ -> a + 1
            | PolyIntOption Some _ -> a + 1
            | PolyFloatOption Some _ -> a + 1
            | _ -> a)
            0
            args
    with
        | 0 -> print_endline "Error: no operation specified"; exit 2
        | 1 -> ()
        | _ -> print_endline "Only one operation can be specified at a time"; exit 2


let main () =
    read_env_vars ();
    parse cmd_specs cmd_anon usage_msg;
    check_cmdline ();
    match !set_name with
        Some n -> if set_package_name n then exit 0 else exit 1
    | None -> match !set_version with
        Some v -> if set_package_version v then exit 0 else exit 1
    | None -> match !set_architecture with
        Some a -> if set_package_architecture a then exit 0 else exit 1
    | None -> match !show_missing with
        Some () -> if show_missing_information () then exit 0 else exit 1
    | None -> match !print_version with
        Some () -> print_endline version_msg; exit 0
    | None -> match !create_desc_type with
        Some t -> if create_package t then exit 0 else exit 1
    | None -> match !add_files with
        Some () -> if add_files_from_destdir () then exit 0 else exit 1
    | None -> match !add_dependency with
        Some s -> if pkg_add_dependency s then exit 0 else exit 1
    | None -> match !remove_dependencies with
        Some () -> if pkg_remove_dependencies () then exit 0 else exit 1
    | None -> match !pack with
        Some () -> if create_packed_form () then exit 0 else exit 1
    | None -> match !install with
        Some () -> if !anon_args = []
            then (print_endline "--install requires an argument"; exit 2)
            else if install_packages_ui !anon_args then exit 0 else exit 1
    | None -> match !policy with
        | Some n -> (* if show_policy n then exit 0 else *) exit 1
    | None -> match !remove with
        Some () -> (* if !anon_args = []
            then (print_endline "--remove requires an argument"; exit 2)
            else if remove_packages_ui !anon_args then exit 0 else *) exit 1
    | None -> match !list_installed with
        | Some () -> if list_installed_packages () then exit 0 else exit 1
    | None -> match !show_problems with
        | Some () -> if show_problems_with_installation () then exit 0 else exit 1
    | None -> match !recover with
        | Some () -> (* if recover_from_dirty_state () then exit 0 else *) exit 1
    | None -> match !installation_graph with
        | Some () -> if print_igraph !anon_args then exit 0 else exit 1
    | None -> match !reverse_dependencies with
        | Some n -> (* (try print_reverse_dependencies n; exit 0 with _ -> *) exit 1
    | None -> match !op_mark_manual with
        | Some () -> if !anon_args = []
            then (print_endline "--mark-manual requires an argument"; exit 2)
            else if mark_manual_ui !anon_args then exit 0 else exit 1
    | None -> match !op_mark_auto with
        | Some () -> if !anon_args = []
            then (print_endline "--mark-auto requires an argument"; exit 2)
            else if mark_auto_ui !anon_args then exit 0 else exit 1
    | None -> match !op_show_version with
        | Some name -> if ui_show_version name then exit 0 else exit 1
    | None ->
        print_endline "Something went wrong (you should not see this): no command specified (!?)"

let () = main ()