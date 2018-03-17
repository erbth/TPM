let target_system = ref Tpm_config.default_target_system

let print_target () =
    if !target_system <> Tpm_config.default_target_system
    then
        print_endline ("Target system is at \"" ^ !target_system ^ "\" (not the default)")
    else
        ()

type poly_option_wrapper =
    PolyIntOption of int option |
    PolyStringOption of string option |
    PolyUnitOption of unit option |
    PolyFloatOption of float option

type arch = I386 | Amd64
let arch_of_string = function
    | "i386" -> Some I386
    | "amd64" -> Some Amd64
    | _ -> None

let string_of_arch = function
    | I386 -> "i386"
    | Amd64 -> "amd64"

type version = (int * int * int)
let version_of_string s =
    let ss = String.split_on_char '.' s
    in
    match ss with
        | [] -> None
        | major::ss -> (
        match ss with
            | [] -> (
                try Some (int_of_string major,0,0)
                with  _ -> None
            )
            | minor::ss -> (match ss with
                | [] -> (
                    try Some (int_of_string major, int_of_string minor,0)
                    with _ -> None
                )
                | revision::_ -> (
                    try Some (int_of_string major, int_of_string minor, int_of_string revision)
                    with _ -> None
                )
            )
        )

let string_of_version (major,minor,revision) =
    string_of_int major ^ "." ^
    string_of_int minor ^ "." ^ string_of_int revision

let add_xml_descriptor = (^) "<?xml version=\"1.0\"?>\n"
let xml_to_string_with_desc s = (Xml.to_string_fmt s |> add_xml_descriptor) ^ "\n"

let compare_names a b = if a > b then 1 else if a < b then -1 else 0

let contains l e = List.exists (fun l -> l = e) l

(* Difference of sorted lists: l1 \ l2 *)
let sorted_difference cmp l1 l2 =
    List.filter
        (fun e -> not (contains l2 e))
        l1