open Util
open Pkg

let create_package t =
    print_target ();
    match pkg_type_of_string t with
        | None -> print_endline "Invalid package type"
        | Some t -> (
            let p = { empty_pkg with t = Some t }
            in
                xml_of_pkg p
        )