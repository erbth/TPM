open Repository
open Configuration
open Util

(* val find_package : repository -> string -> Pkg.pkg list *)

(* val find_package_in_all_repos :
    string -> arch -> (repository * Pkg.static_pkg) list *)

val find_and_select_package_in_all_repos :
    string -> package_constraint list -> arch -> (repository * Pkg.static_pkg) option

(* val find_package_version_in_all_repos :
    string -> version -> arch -> (repository * Pkg.static_pkg) option *)
