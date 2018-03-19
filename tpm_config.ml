(* This file must not be confused with the runtime configuration provided through
 * in configuration.ml. This file is like autotool's config.h in C. *)

let version = (1,0,0)
let default_target_system = "/"
let desc_file_name = "desc.xml"
let destdir_name = "destdir"
let program_sha512sum = "sha512sum"
let program_cut = "cut"
let program_tar = "tar"
let program_cd = "cd"
let program_gzip = "gzip"
let conf_path_prefixes = [
    Str.regexp "/etc"
]
let tmp_dir = "/tmp/tpm"

let config_file_path = "/etc/tpm/config.xml"
let status_file_path = "/var/lib/tpm/status.xml"