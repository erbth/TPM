open Repository
open Configuration
open Util

val find_and_select_package_in_all_repos :
    string -> package_constraint list -> arch ->
    (repository * Pkg.static_pkg) option

val find_and_filter_package_in_all_repos :
    string -> package_constraint list -> arch ->
    (repository * Pkg.static_pkg) list
