open Arg
open Util
open Unpacked_package

let version_msg =
    let (major,minor,revision) = Tpm_config.version
    in
        ("TSClient LEGACY Package Manager version " ^
        (string_of_int major) ^ "." ^
        (string_of_int minor) ^ "." ^
        (string_of_int revision))

let cmd_version () = print_endline version_msg

let usage_msg = version_msg

let cmd_specs = [
    ("--version", Unit cmd_version, "List the program's version");
    ("--target", Set_string target_system, "Root of the managed system's filesystem");
    ("--create-desc", String create_package,
        "Create desc.xml with package type and destdir in the current working " ^
        "directory")
]

let cmd_anon a =
    print_endline ("Invalid option \"" ^ a ^ "\"");
    exit 2

let main () =
    parse cmd_specs cmd_anon usage_msg;
    print_endline "Error: no operation specified";
    exit 2

let () = main ()