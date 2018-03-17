module Swdb = struct
    open Sys
    open List
    open Xml

    exception Swdb_error of string

    type sw =   Unresolved_sw_name of string |
                Executable         of { name:string; runtime_deps:sw list } |
                Library            of { name:string; runtime_deps:sw list } |
                Package            of { name:string; runtime_deps:sw list }

    type sw_db = sw list

    let is_xml_file n =
        let ss = String.split_on_char '.' n
        in
            let s = ss |> rev |> hd
            in
                if s = "xml" then true else false

    let create_empty () = []

    let interpret_xml_sw xs =
        (* name, type (as string), runtime deps *)
        let ps = (None, None, [])
        in
        let ps = fold_left
            (fun ps x -> match x with
              | Element (t,_,c) when t = "name" -> (match c with
                  | PCData n :: cs -> let (_,t,d) = ps in (Some n,t,d)
                  | _ -> ps)

              | Element (t,_,c) when t = "type" -> (match c with
                  | PCData t :: cs -> let (n,_,d) = ps in (n,Some t,d)
                  | _ -> ps)

              | Element (t,_,c) when t = "run_time_dependencies" -> (
                    fold_left (fun ps x -> match x with
                      | Element (a,_,c) when a = "dep" -> (match c with
                          | PCData d :: cs -> let (n,t,ds) = ps in (n,t, Unresolved_sw_name d :: ds)
                          | _ -> ps)
                      | _ -> ps)
                        ps c)

              | _ -> ps)
            ps
            xs
        in
            let s = match ps with
              | (Some n, Some t, ds) -> (match t with
                  | "package"    -> Package { name=n; runtime_deps=ds }
                  | "library"    -> Library { name=n; runtime_deps=ds }
                  | "executable" -> Executable { name=n; runtime_deps=ds }
                  | _ -> raise (Swdb_error ("Invalid software type \"" ^ t ^"\"")))
              | _ -> raise (Swdb_error "Name or type missing")
            in
                s


    let add_from_xml_file db name =
        if is_xml_file name
        then
            (print_endline ("Reading file \"" ^ name ^ "\"");
            let x = parse_file name
            in
                let work db = function
                  | Element (t,a,c) -> (match t with
                      | "sw" -> interpret_xml_sw c :: db
                      |  _   -> print_endline ("File \"" ^ name ^ ": Invalid tag \"" ^ t ^ "\" at toplevel"); db)
                  | _ -> print_endline ("File \"" ^ name ^ ": Data at toplevel"); db
                in
                    work db x
            )
        else
            db

    let add_from_file db n =
        if (is_directory n)
        then
            fold_left
                (fun a fn -> add_from_xml_file a (n ^ fn))
                db
                (n |> readdir |> Array.to_list)
        else
            add_from_xml_file db n


    let get_runtime_dependencies db ss =
        let deps = fold_left
            (fun ds s -> (
                match s with
                  | Unresolved_sw_name -> ds
                  | _ -> (fold_left
                        (fun ds s -> if not (exists (fun s_ -> s = s_) ds) then s::ds else ds)
                        []
                        cs
        in
            deps

    let get_unresolved_sw_names ss =
        fold_left
            (fun ns s -> match s with Unresolved_sw_name n -> n::ns | _ -> ns)
            []
            ss
end
