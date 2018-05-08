(* This file must not be confused with the runtime configuration provided through
 * in configuration.ml. This file is like autotool's config.h in C. *)

type runtime_system = Native_runtime | Directory_runtime of string

let version = (1,0,5)
let default_runtime_system = Native_runtime

let desc_file_name = "desc.xml"
let desc_file_version = "1.1"

let destdir_name = "destdir"
let configuresh_name = "configure.sh"
let unconfiguresh_name = "unconfigure.sh"
let package_info_location = "/var/lib/tpm"
let default_program_sha512sum = "sha512sum"
let default_program_tar = "tar"
let default_program_gzip = "gzip"
let conf_path_prefixes = [
    Str.regexp "/etc"
]
let tmp_dir = "/tmp/tpm"

let cp_buffer_size = 8192

let config_file_path = "/etc/tpm/config.xml"
let config_file_version = "1.0"

let status_file_path = "/var/lib/tpm/status.xml"
let status_file_version = "1.1"
