let target_system = ref Tpm_config.default_target_system

let print_target () =
    print_endline ("Target system is at \"" ^ !target_system ^ "\"" ^
        if !target_system <> Tpm_config.default_target_system
        then " (not the default)" else "")

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

let version_bigger (maj1,min1,rev1) (maj2,min2,rev2) =
    maj1 > maj2 || maj1 = maj2 && min1 > min2 ||
    maj1 = maj2 && min1 = min2 && rev1 > rev2

let add_xml_descriptor = (^) "<?xml version=\"1.0\"?>\n"
let xml_to_string_with_desc s = (Xml.to_string_fmt s |> add_xml_descriptor) ^ "\n"

let compare_names a b = if a > b then 1 else (if a < b then -1 else 0)

let contains l e = List.exists (fun l -> l = e) l

(* Difference of sorted lists: l1 \ l2 *)
let sorted_difference cmp l1 l2 =
    List.filter
        (fun e -> not (contains l2 e))
        l1

let sorted_unique_insert cmp l e =
    let rec before e src dst =
        (match src with
            | [] -> e::dst
            | l::ls ->
                if cmp l e < 0
                then before e ls (l::dst)
                else
                    if cmp l e = 0
                    then after ls (l::dst)
                    else after ls (l::e::dst))
        and after src dst = List.fold_left (fun a x -> x::a) dst src
    in
        before e l [] |> List.rev

(* Takes an absolute path *)
let form_target_path p = !target_system ^ p

let create_tmp_dir () =
    if not (try Sys.is_directory Tpm_config.tmp_dir with _ -> false)
    then
        try Unix.mkdir Tpm_config.tmp_dir 0o755; true
        with Unix.Unix_error (c,_,_) ->
            print_endline ("Could not create the temporary directory \"" ^
            Tpm_config.tmp_dir ^ "\": " ^ Unix.error_message c); false
    else true

(* Potentially raises a Unix_error *)
let mkdir_p_at_target dir perm =
    let rec work cd = function
        | [] -> ()
        | d::dirs ->
            let cd = cd ^ "/" ^ d
            in
            (if d <> "" && not (Sys.file_exists cd)
            then
                (let pu = Unix.umask 0o000
                in Unix.mkdir cd perm;
                let _ = Unix.umask pu
                in ())
            else ());
            work cd dirs
    in
    let dirs = String.split_on_char '/' dir
    in
    work !target_system dirs

let print_failed () =
    print_endline (" [" ^ Terminal.red ^ "failed" ^ Terminal.normal ^ "]")

let print_ok () =
    print_endline (" [  " ^ Terminal.green ^ "OK" ^ Terminal.normal ^ "  ]")
