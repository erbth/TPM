(* This file must not be confused with the runtime configuration provided through
 * in configuration.ml. This file is like autotool's config.h in C. *)

let version = (1,0,2)
let default_target_system = "/"
let desc_file_name = "desc.xml"
let destdir_name = "destdir"
let postinstsh_name = "postinst.sh"
let configuresh_name = "configure.sh"
let preupdatesh_name = "preupdate.sh"
let postupdatesh_name = "postupdate.sh"
let prermsh_name = "prerm.sh"
let package_info_location = "/var/lib/tpm"
let default_program_sha512sum = "sha512sum"
let default_program_tar = "tar"
let default_program_cd = "cd"
let default_program_gzip = "gzip"
let default_program_install = "install"
let conf_path_prefixes = [
    Str.regexp "/etc"
]
let tmp_dir = "/tmp/tpm"

let config_file_path = "/etc/tpm/config.xml"
let status_file_path = "/var/lib/tpm/status.xml"
