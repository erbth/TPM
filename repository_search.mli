open Repository

val find_package : repository -> string -> Pkg.pkg list
val find_package_in_all_repos : string -> (repository * Pkg.pkg) list