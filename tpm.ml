open Arg
open Util
open Pkg
open Unpacked_package
open Packed_package
open Repository_search
open Configuration
open Installation

let select_version_to_install =
    List.fold_left
        (fun a rp -> match a with None -> Some rp | Some a ->
            if pkg_newer (snd rp) (snd a) then Some rp else Some a)
        None

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
    List.fold_left
        (fun s (r,p) -> if s
            then install_or_upgrade_package r p Status.Manual
            else false)
        true
        rps

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
    try target_system := Unix.getenv "TPM_TARGET" with _ -> ()

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
    ("--install", Unit cmd_install, "Install or update the specified packages; " ^
        "<name> is a regular expression in the format used by OCaml's Str module.")
]

let anon_args = ref []
let cmd_anon a =
    if !install = Some () then anon_args := a::!anon_args
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
        PolyUnitOption !install
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
    | None ->
        print_endline "Something went wrong (you should not see this): no command specified (!?)"

let () = main ()