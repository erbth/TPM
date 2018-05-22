open Pkg
open Hashtbl
open Util

type pkgdb = (string, static_pkg list) Hashtbl.t

let create_empty () : pkgdb =
    Hashtbl.create ~random:true 1000

let add_spkg (db : pkgdb) (spkg : static_pkg) =
    let l =
        match Hashtbl.find_opt db spkg.sn with
            | None -> []
            | Some l -> l
    in
    Hashtbl.replace db spkg.sn (spkg::l)

let read (filepath : string) =
    let process_element (db : pkgdb) e =
        match pkg_of_xml e with
            | None ->
                failwith "pkg_of_xml failed"
            | Some pkg ->
        match static_of_dynamic_pkg pkg with
            | None ->
                failwith "static_of_dynamic_pkg failed"
            | Some spkg ->
        add_spkg db spkg
    in
    let process_elements es =
        let db = create_empty ()
        in
        List.fold_left
            (fun () e -> process_element db e)
            ()
            es;
        db
    in

    try
        let x =
            Xml.parse_file filepath
        in
        match x with
            | Xml.PCData _ ->
                failwith "Data on toplevel, which is invalid"
            | Xml.Element ("pkgdb", attrs, children) ->
                (match List.assoc_opt "file_version" attrs with
                    | None ->
                        failwith "File_version attribute is missing"
                    | Some v when v = Tpm_config.desc_file_version ->
                        Some (process_elements children)
                    | Some v ->
                        failwith ("Invalid db file version \"" ^ v ^ "\""))

            | Xml.Element (n, _, _) ->
                failwith ("Invalid toplevel element \"" ^ n ^ "\"")
    with
        | Failure msg
        | Sys_error msg ->
            print_endline ("Pkgdb.read: " ^ msg);
            None
        | _ ->
            print_endline "Pkgdb.read failed";
            None

let write (db : pkgdb) (filepath : string) =
    try
        let xml_pkgs =
            Hashtbl.fold
                (fun _ spkgs l ->
                    List.fold_left
                        (fun l spkg ->
                            match
                                (dynamic_of_static_pkg spkg |> xml_of_pkg)
                            with
                                | None -> failwith "xml_of_pkg failed"
                                | Some x -> x :: l)
                        l
                        spkgs)
                db
                []
        in
        let attrs =
            [ ("file_version", Tpm_config.desc_file_version) ]
        in
        let x =
            Xml.Element ("pkgdb", attrs, xml_pkgs)
        in

        let oc = open_out filepath
        in

        xml_to_string_with_desc x |> output_string oc;
        close_out oc;
        true
    with
        | Failure msg
        | Sys_error msg ->
            print_endline ("pkgdb.write: " ^ msg);
            false
        | _ ->
            print_endline ("pkgdb.write failed");
            false

(* Varios SQL SELECT-like queries *)
let select_name_version_arch spp fp (db : pkgdb) =
    let in_files sp =
        let files = sp.sfiles
        in
        let files =
            List.fold_left
                (fun l (_, f) -> f::l)
                files
                sp.scfiles
        in
        List.exists
            fp
            files
    in
    Hashtbl.fold
        (fun n sps o ->
            List.filter
                (fun sp ->
                    if spp sp
                    then in_files sp
                    else false)
                sps @ o)
        db
        []
    |> List.map (fun sp -> (sp.sn, sp.sv, sp.sa))

let select_name_version_arch_in_latest_version spp fp (db : pkgdb) =
    let in_files sp =
        let files = sp.sfiles
        in
        let files =
            List.fold_left
                (fun l (_, f) -> f::l)
                files
                sp.scfiles
        in
        List.exists
            fp
            files
    in
    Hashtbl.fold
        (fun n sps o ->
            match
                list_max
                    (fun sp1 sp2 -> compare_version sp1.sv sp2.sv)
                    sps
            with
                | None -> o
                | Some sp ->
                    if spp sp
                    then (if in_files sp then sp :: o else o)
                    else o)
        db
        []
    |> List.map (fun sp -> (sp.sn, sp.sv, sp.sa))

let select_spkgs spp (db : pkgdb) =
    Hashtbl.fold
        (fun _ sps l ->
            List.filter (fun sp -> if spp sp then true else false) sps @ l)
        db
        []

let select_latest_versioned_spkgs spp (db : pkgdb) =
    Hashtbl.fold
        (fun _ sps l ->
            match
                list_max (fun sp1 sp2 -> compare_version sp1.sv sp2.sv) sps
            with
                | None -> l
                | Some sp ->
                    if spp sp then sp :: l else l)
        db
        []
