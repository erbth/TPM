type configuration = {
    repos:Repository.repository list;
    a:Util.arch
}

val read_configuration : unit -> configuration option