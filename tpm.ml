open Arg
open Util
open Pkg
open Unpacked_package
open Packed_package
open Installed_package
open Repository_search
open Configuration
open Installation
open Status

let install_packages names =
    print_target ();
    match read_configuration () with
        | None -> false
        | Some cfg ->
    let rps =
        List.fold_left
            (fun a name -> match a with None -> None | Some a ->
                let rps =
                    find_package_in_all_repos name |>
                    List.filter (fun rp ->
                        let (_,p) = rp
                        in p.Pkg.a = Some cfg.Configuration.a)
                in
                match select_version_to_install rps with
                    | None -> print_endline ("Package \"" ^ name ^
                        "\" not found for architecture " ^ (string_of_arch cfg.a));
                        None
                    | Some rp -> Some (rp::a))
            (Some [])
            names
    in
    match rps with None -> false | Some rps ->
    match read_status () with None -> false | Some status ->
    if check_installation status true|> not then false
    else
    let status =
        List.fold_left
            (fun s (r,p) -> match s with None -> None | Some s ->
                install_or_upgrade_package s r p Status.Manual)
            (Some status)
            rps
    in
    match status with None -> false | Some _ -> true

let remove_packages names =
    print_target();
    match read_status () with None -> false | Some status ->
    let names =
        List.fold_left
            (fun ns n ->
                match select_status_tupel_by_name status n with
                    | None -> print_endline
                        ("Package " ^ n ^ " is not installed");
                        ns
                    | Some _ -> n::ns)
            []
            (List.rev names)
    in
    if check_installation status true |> not then false
    else
    let status =
        List.fold_left
            (fun s name -> match s with None -> None | Some s ->
                remove_package s name)
            (Some status)
            (List.rev names)
    in
    match status with None -> false | Some _ -> true

let recover_by_removing () =
    print_target ();
    match read_status () with None -> false | Some status ->
    let forcefully_remove_packages status pkgs =
        List.fold_left
            (fun status pkg -> match status with None -> None | Some status ->
                match pkg.n with
                    | None -> print_endline "Package with no name, aborting";
                        None
                    | Some n -> force_remove status n)
            (Some status)
            pkgs
    in
    get_dirty_packages status false
    |> forcefully_remove_packages status
    |> bool_of_option

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
    try target_system := Unix.getenv "TPM_TARGET" with _ -> ();
    try program_sha512sum := Unix.getenv "TPM_PROGRAM_SHA512SUM" with _ -> ();
    try program_tar := Unix.getenv "TPM_PROGRAM_TAR" with _ -> ();
    try program_cd := Unix.getenv "TPM_PROGRAM_CD" with _ -> ();
    try program_gzip := Unix.getenv "TPM_PROGRAM_GZIP" with _ -> ()

(* Set TPM_TARGET for the packaging scripts *)
let put_env_vars () =
    try
        Unix.putenv "TPM_TARGET" !target_system
    with
        | Unix.Unix_error (c,_,_) -> print_endline ("Can not set \"TPM_TARGET\" " ^
            "in the processes environment: " ^ Unix.error_message c); exit 1
        | _ -> print_endline ("Can not set \"TPM_TARGET\" in the processes " ^
            "environment."); exit 1

(* Commands *)
let create_desc_type = ref None
let cmd_create_desc s = create_desc_type := Some s

let print_version = ref None
let cmd_print_version () = print_version := Some ()

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

let add_rdependency = ref None
let cmd_add_rdependency s = add_rdependency := Some s

let remove_rdeps = ref None
let cmd_remove_rdeps () = remove_rdeps := Some ()

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

let cmd_specs = [
    ("--version", Unit cmd_print_version, "Print the program's version");
    ("--target", Set_string target_system, "Root of the managed system's filesystem");
    ("--create-desc", String cmd_create_desc,
        "Create desc.xml with package type and destdir in the current working " ^
        "directory");
    ("--show-missing", Unit cmd_show_missing,
        "List missing essential informatin in the package description");
    ("--set-name", String cmd_set_name, "Set the package's name");
    ("--set-version", String cmd_set_version, "Set the package's version");
    ("--set-arch", String cmd_set_architecture, "Set the package's architecture");
    ("--add-files", Unit cmd_add_files, "Add files in destdir");
    ("--add-rdependency", String cmd_add_rdependency, "Add a runtime dependency");
    ("--remove-rdependencies", Unit cmd_remove_rdeps, "Remove all runtime dependencies");
    ("--pack", Unit cmd_pack, "Create the packed/transport form of the package");
    ("--install", Unit cmd_install, "Install or uprade the specified packages");
    ("--policy", String cmd_policy, "Show the installed and available versions of name");
    ("--remove", Unit cmd_remove, "Remove the specified packages and their " ^
        "config files if they were not modified");
    ("--list-installed", Unit cmd_list_installed, "List all installed packages");
    ("--show-problems", Unit cmd_show_problems, "Show all problems with the current " ^
        "installation (i.e. halfly installed packages after an interruption or " ^
        "missing dependencies)");
    ("--recover", Unit cmd_recover, "Recover from a dirty state by deleting all " ^
        "packages that are in a dirty state (always possible due to atomic " ^
        "write operations to status")
]

let anon_args = ref []
let cmd_anon a =
    if !install = Some () || !remove = Some ()
    then anon_args := a::!anon_args
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
        PolyStringOption !add_rdependency;
        PolyUnitOption !remove_rdeps;
        PolyUnitOption !pack;
        PolyUnitOption !install;
        PolyStringOption !policy;
        PolyUnitOption !remove;
        PolyUnitOption !list_installed;
        PolyUnitOption !show_problems;
        PolyUnitOption !recover
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
    put_env_vars ();
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
    | None -> match !add_rdependency with
        Some s -> if add_runtime_dependency s then exit 0 else exit 1
    | None -> match !remove_rdeps with
        Some () -> if remove_runtime_dependencies () then exit 0 else exit 1
    | None -> match !pack with
        Some () -> if create_packed_form () then exit 0 else exit 1
    | None -> match !install with
        Some () -> if !anon_args = []
            then (print_endline "--install requires an argument"; exit 2)
            else if install_packages !anon_args then exit 0 else exit 1
    | None -> match !policy with
        | Some n -> if show_policy n then exit 0 else exit 1
    | None -> match !remove with
        Some () -> if !anon_args = []
            then (print_endline "--remove requires an argument"; exit 2)
            else if remove_packages !anon_args then exit 0 else exit 1
    | None -> match !list_installed with
        | Some () -> if list_installed_packages () then exit 0 else exit 1
    | None -> match !show_problems with
        | Some () -> if show_problems_with_installation () then exit 0 else exit 1
    | None -> match !recover with
        | Some () -> if recover_by_removing () then exit 0 else exit 1
    | None ->
        print_endline "Something went wrong (you should not see this): no command specified (!?)"

let () = main ()