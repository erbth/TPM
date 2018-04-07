open Repository
open Configuration

val find_package : repository -> string -> Pkg.pkg list
val find_package_in_all_repos : string -> (repository * Pkg.pkg) list
val find_and_select_package_in_all_repos : string -> (repository * Pkg.pkg) option
val find_and_select_packages_in_all_repos :
    configuration -> string list -> ((repository * Pkg.pkg) list) option