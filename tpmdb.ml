open Util
open Arg
open Tpmdb_common
open Pkg

(* Stuff that must be here due to linear parsing of OCaml files *)
let print_only_names = ref false
let arch_filter = ref None
let only_in_latest_version = ref false

(* Operations *)
let create_from_directory (path : string) =
    let db = Pkgdb.create_empty ()
    in
    let process_package acc path =
        if not (create_tmp_dir ())
        then failwith "tpmdb: Could not create tmp dir"
        else
        let unpack_args =
            [|!program_tar; "-xf"; path;
            "-C"; Tpm_config.tmp_dir; Tpm_config.desc_file_name|]
        in
        try
            match run_program unpack_args with
                | (_, WEXITED 0) ->
                    let pkg =
                        Xml.parse_file
                            (Tpm_config.tmp_dir ^ "/" ^ Tpm_config.desc_file_name)
                        |> pkg_of_xml
                    in
                    (match pkg with
                        | None ->
                            failwith "tpmdb: pkg_of_xml failed"
                        | Some pkg ->

                    match static_of_dynamic_pkg pkg with
                        | None ->
                            failwith "tpmdb: static_of_dynamic_pkg failed"
                        | Some spkg ->
                            Pkgdb.add_spkg db spkg)

                | _ -> failwith 
                    "tpmdb: Could not unpack the package (tar failed)"
        with
            | Unix.Unix_error (c, _, _) -> failwith
                ("tpmdb: Could not read the package: " ^ Unix.error_message c)
            | _ -> failwith ("tpmdb: Could not read the package")
    in

    let to_file db = function
        None -> false | Some _ ->

        Pkgdb.write db !Tpmdb_common.db_file
    in

    fold_directory_tree
        process_package
        ()
        path
    |> to_file db

let find_files patterns =
    let regexps =
        List.map Str.regexp patterns
    in

    match Pkgdb.read !Tpmdb_common.db_file with
        | None ->
            print_endline "Tpmdb: Could not read database.";
            false
        | Some db ->

    let spp = match !arch_filter with
        | None -> fun _ -> true
        | Some a -> fun sp -> sp.sa = a
    in
    let nvas =
        let selector = match !only_in_latest_version with
            | false -> Pkgdb.select_name_version_arch
            | true -> Pkgdb.select_name_version_arch_in_latest_version
        in
        selector
            spp
            (fun fn ->
                List.exists
                    (fun r -> Str.string_match r fn 0)
                    regexps)
            db
    in

    let print_tuple (n, v, a) =
        print_endline
            (if !print_only_names
            then
                n
            else
                (n ^ "=" ^ string_of_version v ^ "@" ^ string_of_arch a))
    in

    let cmp_version = !print_only_names = false
    in
    let cmp_arch =
        !print_only_names = false && !arch_filter <> None
    in
    let nvas =
        List.sort_uniq
            (fun (n1,v1,a1) (n2,v2,a2) ->
                let cmp = compare_names n1 n2
                in
                let cmp =
                    if cmp_version && cmp = 0
                    then compare_version v1 v2
                    else cmp
                in
                let cmp =
                    if cmp_arch && cmp = 0
                    then compare_archs a1 a1
                    else cmp
                in
                cmp)
            nvas
    in
    List.iter print_tuple nvas;
    true

let get_dependencies patterns =
    match Pkgdb.read !Tpmdb_common.db_file with
        | None ->
            print_endline "Tpmdb: Could not read database.";
            false
        | Some db ->

    let patterns =
        List.map (fun p -> Str.regexp p) patterns
    in

    let selector = match !only_in_latest_version with
        | true -> Pkgdb.select_latest_versioned_spkgs
        | false -> Pkgdb.select_spkgs
    in

    let predicate = match !arch_filter with
        | None -> (fun _ -> true)
        | Some a -> (fun sp -> sp.sa = a)
    in

    (* Select by name *)
    let predicate =
        fun sp -> predicate sp &&
            List.exists
                (fun r -> Str.string_match r sp.sn 0)
                patterns
    in

    let deps =
        selector predicate db
        |> List.map (fun sp -> sp.sdeps)
        |> List.fold_left (fun a l -> List.map (fun (n,cs) -> n) l @ a) []
        |> List.sort_uniq compare_names
    in

    List.iter print_endline deps;
    true

let get_reverse_dependencies patterns =
    match Pkgdb.read !Tpmdb_common.db_file with
        | None ->
            print_endline "Tpmdb: Could not read database.";
            false
        | Some db ->

    let res = List.map Str.regexp_string patterns
    in

    let selector = match !only_in_latest_version with
        | true -> Pkgdb.select_latest_versioned_spkgs
        | false -> Pkgdb.select_spkgs
    in

    let arch_predicate = match !arch_filter with
        | None -> (fun _ -> true)
        | Some a -> (fun sp -> sp.sa = a)
    in

    (* Select by name *)
    let predicate =
        fun sp -> arch_predicate sp &&
            List.exists
                (fun r -> Str.string_match r sp.sn 0)
                res
    in

    let refpkgnames =
        selector predicate db
        |> List.map (fun sp -> sp.sn)
    in

    let rdep_predicate =
        fun sp -> arch_predicate sp &&
            List.map
                (fun (dep,_) -> dep)
                sp.sdeps
            |> List.exists
                (fun dep ->
                    List.exists (fun ref -> ref = dep) refpkgnames)
    in

    let rdeps =
        selector rdep_predicate db
        |> List.sort_uniq (fun sp1 sp2 -> compare_names sp1.sn sp2.sn)
    in

    let print_fkt =
        match !print_only_names with
            | true -> fun sp -> print_endline sp.sn
            | false -> fun sp ->
                print_endline
                    (sp.sn ^ "=" ^
                    string_of_version sp.sv ^ "@" ^
                    string_of_arch sp.sa)
    in

    List.iter print_fkt rdeps;
    true

(* User interface *)
let version_msg =
    let (major,minor,revision) = Tpm_config.version
    in
        ("Package database tool of the TSClient LEGACY Package Manager version " ^
        (string_of_int major) ^ "." ^
        (string_of_int minor) ^ "." ^
        (string_of_int revision))

let usage_msg = version_msg

(* Commands and options *)
let op_print_version = ref None
let cmd_print_version () = op_print_version := Some ()

let cmd_db_file (f : string) =
    db_file := f

let op_create_from_directory = ref None
let cmd_create_from_directory d = op_create_from_directory := Some d

let op_find_files = ref None
let cmd_find_files () = op_find_files := Some ()

let op_get_dependencies = ref None
let cmd_get_dependencies () = op_get_dependencies := Some ()

let op_get_reverse_dependencies = ref None
let cmd_get_reverse_dependencies () = op_get_reverse_dependencies := Some ()

let cmd_arch_filter s =
    match arch_of_string s with
        | None ->
            print_endline ("Invalid architecture \"" ^ s ^ "\"");
            exit 2
        | Some a ->
            arch_filter := Some a

let cmd_specs = [
    ("--version", Unit cmd_print_version, "Print the program's version");
    ("--db", String cmd_db_file, "The database file to use");
    ("--create-from-directory", String cmd_create_from_directory,
        "Create a database by recursively searching the given directory for " ^
        "TSL packages");
    ("--find-files", Unit cmd_find_files, "Find a file in the database and " ^
        "print the package it belongs to.");
    ("--arch", String cmd_arch_filter, "Filter the result by architecture");
    ("--print-only-names", Set print_only_names, "Print only package names");
    ("--only-in-latest-version", Set only_in_latest_version, "Search only in " ^
     "the latest version of each package");
    ("--get-dependencies", Unit cmd_get_dependencies, "Print the specified " ^
     "packages' direct dependencies");
    ("--get-reverse-dependencies", Unit cmd_get_reverse_dependencies, "Same as " ^
     "--get-dependencies but retrieves the immediate reverse dependencies of a package")
]

let anon_args = ref []
let cmd_anon a =
    if
        !op_find_files = Some () ||
        !op_get_dependencies = Some () ||
        !op_get_reverse_dependencies = Some ()
    then anon_args := (a::(!anon_args |> List.rev)) |> List.rev
    else(print_endline ("Invalid option \"" ^ a ^ "\""); exit 2)

let check_cmdline () =
    let args = [
        PolyUnitOption !op_print_version;
        PolyStringOption !op_create_from_directory;
        PolyUnitOption !op_find_files;
        PolyUnitOption !op_get_dependencies;
        PolyUnitOption !op_get_reverse_dependencies
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
    parse cmd_specs cmd_anon usage_msg;
    check_cmdline ();
    match !op_print_version with
        Some () -> print_endline version_msg; exit 0
    | None -> match !op_create_from_directory with
        Some n -> if create_from_directory n then exit 0 else exit 1
    | None -> match !op_find_files with
        Some () -> if find_files !anon_args then exit 0 else exit 1
    | None -> match !op_get_dependencies with
        Some () -> if get_dependencies !anon_args then exit 0 else exit 1
    | None -> match !op_get_reverse_dependencies with
        Some () -> if get_reverse_dependencies !anon_args then exit 0 else exit 1
    | None ->
        print_endline "Something went wrong (you should not see this): no command specified (!?)"

let () = main ()
