let target_system = ref Tpm_config.default_target_system

let print_target () =
    if !target_system <> Tpm_config.default_target_system
    then
        print_endline ("Target system is at \"" ^ !target_system ^ "\" (not the default)")
    else
        ()

type arch = I386 | Amd64
let arch_of_string = function
    | "i386" -> Some I386
    | "amd64" -> Some Amd64
    | _ -> None

let string_of_arch = function
    | I386 -> "i386"
    | Amd64 -> "amd64"