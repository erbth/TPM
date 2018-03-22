open Repository
open Pkg
open Status

val install_or_upgrade_package :
    status -> repository -> pkg -> installation_reason -> status option

val remove_package : status -> string -> status option
val show_policy : string -> bool