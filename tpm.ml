open Arg
open Util
open Pkg
open Unpacked_package
open Installed_package
open Repository
open Repository_search
open Configuration
open Status
open Depres

let install_packages_ui names reinstall =
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
                        Some ((n, cs, Manual, reinstall) :: a))
            (Some [])
            names
    with
        | None -> false
        | Some ncrrl ->
    match build_igraph cfg status ncrrl with
        | None -> false
        | Some ig ->
    install_configure_from_igraph cfg status ig
    |> bool_of_option

let remove_packages_ui names =
    print_target ();
    match read_configuration () with
        | None -> false
        | Some cfg ->
    match read_status () with
        | None -> false
        | Some status ->
    let names =
        List.filter
            (fun name ->
                match select_status_tuple_by_name status name with
                    | Some _ -> true
                    | None ->
                        print_endline
                            ("Package \"" ^ name ^
                            "\" is not installed.");
                        false)
            names
    in
    match build_igraph cfg status [] with
        | None -> false
        | Some ig ->
    remove_from_igraph status ig names false
    |> bool_of_option

let remove_unneeded_packages_ui () =
    print_target ();
    match read_configuration () with
        | None -> false
        | Some cfg ->
    match read_status () with
        | None -> false
        | Some status ->
    match build_igraph cfg status [] with
        | None -> false
        | Some ig ->
    match unneeded_packages_from_igraph ig with
        | None -> false
        | Some names ->
    remove_from_igraph status ig names false
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

let recover_from_dirty_state () =
    print_target ();
    print_endline "Recovering:";
    match read_configuration () with
        | None -> false
        | Some cfg ->
    match read_status () with
        | None -> false
        | Some status ->
    
    let remove_packages (status : status option) =
        match status with None -> None | Some status ->
        print_endline "--- Removing packages that must be removed ---";
        match
            List.fold_left
                (fun names (p, _, s) ->
                    match names with None -> None | Some names ->
                    match s with
                        | Installed
                        | Changing
                        | Changing_unconf
                        | Configuring
                        | Configured -> Some names
                        | Removing_unconf
                        | Removing
                        | Installing ->
                            match p.n with
                                | None ->
                                    print_endline "Invalid package in status";
                                    None
                                | Some n ->
                                    Some (n::names))
                (Some [])
                (select_all_status_tuples status)
        with
            | None -> None
            | Some names ->
        print_endline "The following packages will be removed:";
        List.iter
            (fun n -> print_endline ("    " ^ n))
            (List.sort compare_names names);
        match build_igraph cfg status [] with
            | None -> None
            | Some ig ->
        let status =
            remove_from_igraph status ig names true
        in
        print_newline ();
        status
    in

    let install_and_configure_packages (status : status option) =
        match status with None -> None | Some status ->
        print_endline
            "--- Installing and configuring packages if possible ---";
        match build_igraph cfg status [] with
            | None -> None
            | Some ig ->
        let status =
            install_configure_from_igraph cfg status ig
        in
        print_newline ();
        status
    in

    let check (status : status option) =
        match status with None -> None | Some status ->
        print_endline "--- Checking if problems still exist ---";
        match check_installation status true with
            | No_problem ->
                print_endline "No problems found";
                Some status
            | Non_critical ->
                print_endline "Just noncritical problems found";
                Some status
            | Critical ->
                print_endline "Critical problems found";
                None
    in

    remove_packages (Some status)
    |> install_and_configure_packages
    |> check
    |> bool_of_option

let show_policy name =
    print_target ();
    match read_configuration () with
        | None -> false
        | Some cfg ->
    match read_status () with
        | None -> false
        | Some status ->
    match pkg_name_constraints_of_string name with
        | None -> false
        | Some (name, cs) ->
    let ncrrl =
        [(name, List.map (fun c -> (c, None)) cs, Manual, false)]
    in
    let vroti =
        match build_igraph cfg status ncrrl with
            | None -> None
            | Some ig ->
        version_repo_to_install_from_igraph ig name
    in
    let rsps =
        find_and_filter_package_in_all_repos name cs cfg.a
    in
    let ist =
        select_status_tuple_by_name status name
    in
    let string_of_vroti (v, r) =
        string_of_version v ^
        match r with
            | None -> ""
            | Some r -> " from " ^ string_of_repository r
    in
    let string_of_rsp (r, sp) =
        string_of_version sp.sv ^
        " from " ^ string_of_repository r
    in
    let string_of_st (p,r,_) =
        (match p.v with None -> "???" | Some v -> string_of_version v) ^
        " (" ^ (string_of_installation_reason r) ^ ")"
    in
    print_endline ("Policy for package \"" ^ name ^ "\":");
    print_endline ("  Installed instance:  " ^
        match ist with
            | None -> "---"
            | Some t -> string_of_st t);
    print_endline ("  Instance to install: " ^
        match vroti with
            | None -> "---"
            | Some vro -> string_of_vroti vro);
    print_endline
        ("  All available instances for architectur " ^
        string_of_arch cfg.a ^ ":");
    List.iter (fun rsp -> print_endline ("    " ^ string_of_rsp rsp)) rsps;
    true

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

let op_reverse_dependencies = ref None
let cmd_reverse_dependencies s = op_reverse_dependencies := Some s

let op_mark_manual = ref None
let cmd_mark_manual () = op_mark_manual := Some ()

let op_mark_auto = ref None
let cmd_mark_auto () = op_mark_auto := Some ()

let op_show_version = ref None
let cmd_show_version n = op_show_version := Some n

let op_reinstall = ref None
let cmd_reinstall () = op_reinstall := Some ()

let op_remove_unneeded = ref None
let cmd_remove_unneeded () = op_remove_unneeded := Some ()

let cmd_specs = [
    ("--version", Unit cmd_print_version, "Print the program's version");
    ("--target", String cmd_runtime_system, "Root of the managed system's filesystem");
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
    ("--reinstall", Unit cmd_reinstall, "Like install but reinstalls the specified " ^
        "packages even if the same version is already installed");
    ("--policy", String cmd_policy, "Show the installed and available versions of name");
    ("--show-version", String cmd_show_version, "Print a package's version number " ^
        "or `---' if it is not installed");
    ("--remove", Unit cmd_remove, "Remove the specified packages and their " ^
        "config files if they were not modified");
    ("--remove-unneeded", Unit cmd_remove_unneeded, "Remove all packages that were " ^
        "marked as automatically installed and are not required by other packages " ^
        "that are marked as manually installed");
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
        "List the packages that depend on the specified package directly or " ^
        "indirectly");
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
        !op_mark_auto = Some () ||
        !op_reinstall = Some ()
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
        PolyStringOption !op_reverse_dependencies;
        PolyUnitOption !op_mark_manual;
        PolyUnitOption !op_mark_auto;
        PolyStringOption !op_show_version;
        PolyUnitOption !op_reinstall;
        PolyUnitOption !op_remove_unneeded;
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
            else if install_packages_ui !anon_args false then exit 0 else exit 1
    | None -> match !op_reinstall with
        | Some () -> if !anon_args = []
            then (print_endline "--reinstall requires an argument"; exit 2)
            else if install_packages_ui !anon_args true then exit 0 else exit 1
    | None -> match !policy with
        | Some n -> if show_policy n then exit 0 else exit 1
    | None -> match !remove with
        Some () -> if !anon_args = []
            then (print_endline "--remove requires an argument"; exit 2)
            else if remove_packages_ui !anon_args then exit 0 else exit 1
    | None -> match !op_remove_unneeded with
        | Some () -> if remove_unneeded_packages_ui () then exit 0 else exit 1
    | None -> match !list_installed with
        | Some () -> if list_installed_packages () then exit 0 else exit 1
    | None -> match !show_problems with
        | Some () -> if show_problems_with_installation () then exit 0 else exit 1
    | None -> match !recover with
        | Some () -> if recover_from_dirty_state () then exit 0 else exit 1
    | None -> match !installation_graph with
        | Some () -> if print_igraph !anon_args then exit 0 else exit 1
    | None -> match !op_reverse_dependencies with
        | Some n -> if print_reverse_dependencies n then exit 0 else exit 1
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