open Pkg
open Util

type repository = DirectoryRepository of string

let string_of_repository = function
    | DirectoryRepository p -> "directory repository \"" ^ p ^ "\""

let provide_transport_shape_dir_repo path pkg =
    match (pkg.a, packed_name_of_pkg pkg) with
        | (Some a, Some pn) -> Some (path ^ "/" ^ (string_of_arch a) ^ "/" ^ pn)
        | _ -> print_endline "Repository: Invalid package"; None

let provide_transport_shape repo pkg = match repo with
    | DirectoryRepository path -> provide_transport_shape_dir_repo path pkg