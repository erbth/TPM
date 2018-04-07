open Util
open Xml

type pkg_type = Sw | Conf
let pkg_type_of_string = function
    | "sw" -> Some Sw
    | "conf" -> Some Conf
    | _ -> None

let string_of_pkg_type = function
    | Sw -> "sw"
    | Conf -> "conf"

type pkg = {
    t:pkg_type option;                (* package type *)
    n:string option;                  (* package name *)
    v:version option;                 (* version *)
    a:arch option;                    (* architecture *)
    files:string list;                (* path *)
    cfiles:(string * string) list;    (* sha512sum * path *)
    dirs: string list;                (* path *)
    rdeps:string list;
}

type static_pkg = {
    st:pkg_type;                       (* package type *)
    sn:string;                         (* package name *)
    sv:version;                        (* version *)
    sa:arch;                           (* architecture *)
    sfiles:string list;                (* path *)
    scfiles:(string * string) list;    (* sha512sum * path *)
    sdirs: string list;                (* path *)
    srdeps:string list;
}

let empty_pkg = {
    t = None;
    n = None;
    v = None;
    a = None;
    files = [];
    cfiles = [];
    dirs = [];
    rdeps = []
}

(* Generates a string that uniquely identifies the package *)
let string_of_pkg p = match p.n with
    | Some n -> n
    | _ -> "???"

let static_of_dynamic_pkg pkg =
    match (pkg.t, pkg.n, pkg.v, pkg.a) with
        | (Some t, Some n, Some v, Some a) -> Some { st = t; sn = n; sv = v; sa = a;
            sfiles = pkg.files; scfiles = pkg.cfiles; sdirs = pkg.dirs;
            srdeps = pkg.rdeps }
        | _ -> None

let xml_of_pkg (p:pkg) =
    let xes = []
    in
    let xes = List.fold_left
        (fun a d -> Element ("rdep", [],[ PCData d]) :: a)
        xes
        (List.rev p.rdeps)
    in
    let xes = List.fold_left
        (fun a n -> Element ("dir", [], [PCData n]) :: a)
        xes
        (List.rev p.dirs)
    in
    let xes = List.fold_left
        (fun a (s,n) -> Element ("cfile", [("sha512sum", s)],[ PCData n]) :: a)
        xes
       ( List.rev p.cfiles)
    in
    let xes = List.fold_left
        (fun a n -> Element ("file", [],[ PCData n]) :: a)
        xes
        (List.rev p.files)
    in
    let xes = match p.a with
        | None -> xes
        | Some a -> Element ("arch", [], [PCData (string_of_arch a)]) :: xes
    in
    let xes = match p.v with
        | None -> xes
        | Some v -> Element ("version", [], [PCData (string_of_version v)]) :: xes
    in
    let xes = match p.n with
        | None -> xes
        | Some n -> Element ("name", [], [PCData n]) :: xes
    in
    match p.t with
        | None -> None
        | Some Sw -> Some (Element ("sw", [("file_version","1.0")], xes))
        | Some Conf -> Some (Element ("conf", [("file_version", "1.0")], xes))

let pkg_of_xml x =
    let process_name pkg = function
        | [PCData n] -> Some {pkg with n = Some n}
        | _ -> (print_endline "Invalid name"; None)
    in
    let process_version pkg = function
        | [PCData v] -> (
            try Some {pkg with v = version_of_string v}
            with _ -> (print_endline "Invalid version"; None)
        )
        | _ -> (print_endline "Invalid version"; None)
    in
    let process_arch pkg = function
        | [PCData a] -> (
            try Some {pkg with a = arch_of_string a}
            with _ -> (print_endline "Invalid architecture"; None)
        )
        | _ -> (print_endline "Invalid architecture"; None)
    in
    let process_file pkg = function
        | [PCData f] -> Some {pkg with files = pkg.files @ [f]}
        | _ -> (print_endline "Invalid file"; None)
    in
    let process_cfile pkg attrs = function
        | [PCData cf] -> (
            try Some {pkg with cfiles =
                pkg.cfiles @
                [(List.assoc "sha512sum" attrs, cf)]}
            with _ -> (print_endline ("Config file \"" ^ cf ^ "\" has no sha512 sum."); None)
        )
        | _ -> (print_endline "Invalid config file"; None)
    in
    let process_dir pkg = function
        | [PCData d] -> Some {pkg with dirs = pkg.dirs @ [d]}
        | _ -> (print_endline "Invalid directory"; None)
    in
    let process_rdep pkg = function
        | [PCData d] -> Some {pkg with rdeps = pkg.rdeps @ [d]}
        | _ -> (print_endline "Invalid runtime dependency"; None)
    in
    let process_toplevel_element pkg = function
        | PCData d -> (print_endline ("Invalid toplevel element: \"" ^ d ^ "\""); None)
        | Element (t,attrs,cs) -> (match t with
            | "name" -> process_name pkg cs
            | "version" -> process_version pkg cs
            | "arch" -> process_arch pkg cs
            | "file" -> process_file pkg cs
            | "cfile" -> process_cfile pkg attrs cs
            | "dir" -> process_dir pkg cs
            | "rdep" -> process_rdep pkg cs
            | _ -> (print_endline ("Invalid xml tag: \"" ^ t ^ "\""); None)
        )
    in
    let rec process_toplevel_elements pkg_opt es = match pkg_opt with
        | None -> None
        | Some pkg ->
            match es with
                | e::es -> process_toplevel_elements (process_toplevel_element pkg e) es
                | [] -> Some pkg
    in
    match x with
        | Element (t, attr, cs) ->
            (match pkg_type_of_string t with
                | None -> None
                | Some t -> match List.assoc_opt "file_version" attr with
                    | None -> (print_endline "Desc file version not specified"; None)
                    | Some fv -> if fv <> "1.0"
                        then (print_endline ("Unsupported desc file version: " ^ fv); None)
                        else
                            process_toplevel_elements (Some {empty_pkg with t = Some t}) cs)
        | _ -> None

let information_missing {t=t;n=n;v=v;a=a} = match (t,n,v,a) with
    | (Some _, Some _, Some _, Some _) -> false
    | _ -> true

let read_package () =
    if not (Sys.file_exists Tpm_config.desc_file_name)
    then
        (print_endline ("\"" ^ Tpm_config.desc_file_name ^ "\" does not exist."); None)
    else
    let x = Xml.parse_file Tpm_config.desc_file_name
    in
    match pkg_of_xml x with
        | None -> (print_endline "Invalid desc file format"; None)
        | Some pkg -> Some pkg

let write_package pkg = match xml_of_pkg pkg with
    | None -> print_endline "Internal error: No Xml code was generated"; false
    | Some x ->
        (try
            let f = open_out Tpm_config.desc_file_name
            in
            output_string f (xml_to_string_with_desc x);
            close_out f;
            true
        with _ ->
            print_endline
                ("Could not write to file \"" ^
                Tpm_config.desc_file_name ^ "\".");
            false)

let pkg_newer p1 p2 = match (p1.v, p2.v) with
    | (Some v1, Some v2) -> version_bigger v1 v2
    | _ -> false

(* These functions are here to avoid a circular dependency between Packed_package
 * and Repository *)
let pkg_name_of_packed_name n =
    match String.rindex_opt n '_' with
        | None ->
            print_endline ("Packed: Invalid package name: \"" ^ n ^ "\"");
            None
        | Some i -> Some (String.sub n 0 i)

let packed_name_of_pkg pkg =
    match (pkg.n, pkg.v, pkg.a) with
        | (Some n, Some v, Some a) -> Some (
            n ^ "_" ^
            (string_of_arch a) ^ ".tpm.tar")
        | _ -> None

let compare_pkgs_by_name pkg1 pkg2 =
    match (pkg1.n, pkg2.n) with
        | (Some n1, Some n2) -> compare_names n1 n2
        | _ -> raise (Critical_error "Pkg: uncomparable packages")
