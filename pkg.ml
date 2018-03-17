open Util

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
    v:int option;                     (* version *)
    a:arch option;                    (* architecture *)
    files:(string * string) list;     (* sha512sum * filename *)
    cfiles:(string * string) list;    (* sha512sum * filename *)
    rdeps:string list;
}

let empty_pkg = {
    t = None;
    n = None;
    v = None;
    a = None;
    files = [];
    cfiles = [];
    rdeps = []
}

let xml_of_pkg (p:pkg) =
    if p.t = None then None
    else
    
let pkg_of_xml () = empty_pkg