open Util

type repository = DirectoryRepository of string

val string_of_repository : repository -> string
val provide_transport_shape : repository -> string -> version -> arch -> string option