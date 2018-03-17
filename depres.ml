open Swdb

let () =
    if Array.length Sys.argv <> 2
    then
        print_endline ("Usage: " ^ (Array.get Sys.argv 0) ^ " <filname | directoryname>")
    else
        let db = Swdb.create_empty ()
        in
        let db = Swdb.add_from_file db (Array.get Sys.argv 1)
        in
        let deps = Swdb.get_runtime_dependencies db db
        in
        let _ = Swdb.get_unresolved_sw_names deps |> List.map print_endline
        in
        ()
