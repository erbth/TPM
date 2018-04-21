open Util
open Repository
open Pkg
open Status
open Configuration

val elementary_install_package :
    configuration -> string -> version -> installation_reason ->
    repository -> status option -> status option

val elementary_change_package :
    configuration -> string -> version -> installation_reason ->
    repository -> status option -> status option

val elementary_configure_package :
    string -> status option -> status option

val elementary_remove_package :
    string -> bool -> status option -> status option
