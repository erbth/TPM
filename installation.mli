open Repository
open Pkg
open Status

val install_or_upgrade_package :
    status -> repository -> pkg -> installation_reason -> status option
val configure_package_if_possible : status -> string -> status option
val configure_packages_if_possible_filter : string list -> status option -> status option
val configure_all_packages_filter : status option -> status option
val configure_all_packages : status -> status option

val remove_package : status -> string -> status option
val show_policy : string -> bool