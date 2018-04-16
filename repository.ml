open Pkg
open Util

type repository = DirectoryRepository of string

let string_of_repository = function
    | DirectoryRepository p -> "directory repository \"" ^ p ^ "\""

let provide_transport_shape_dir_repo path name version arch =
        Some (path ^ "/" ^ string_of_arch arch ^ "/" ^
            name ^ "-" ^ string_of_version version ^ "_" ^
            string_of_arch arch ^ ".tpm.tar")

let provide_transport_shape repo name version arch =
    match repo with
        | DirectoryRepository path ->
            provide_transport_shape_dir_repo path name version arch