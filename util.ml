let runtime_system = ref Tpm_config.default_runtime_system
let program_sha512sum = ref Tpm_config.default_program_sha512sum
let program_tar = ref Tpm_config.default_program_tar
let program_gzip = ref Tpm_config.default_program_gzip

(* A Critical error shall never be caught *)
exception Critical_error of string
exception Gp_exception of string

let print_target () =
    match !runtime_system with
        | Native_runtime -> print_endline "Runtime system is native";
        | Directory_runtime d -> print_endline ("Runtime system is at \"" ^
            d ^ "\"")

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

let string_of_opt_arch = function
    | None -> "???"
    | Some a -> string_of_arch a

let compare_archs a1 a2 = match (a1, a2) with
    | (I386, I386)
    | (Amd64, Amd64) -> 0
    | (Amd64, I386) -> 1
    | (I386, Amd64) -> -1

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

let compare_version (maj1,min1,rev1) (maj2,min2,rev2) =
    match maj1 = maj2 with
        | false -> maj1 - maj2
        | true -> match min1 = min2 with
            | false -> min1 - min2
            | true -> match rev1 = rev2 with
                | false -> rev1 - rev2
                | true -> 0

let version_bigger v1 v2 =
    compare_version v1 v2 > 0

type package_constraint =
    No_constraint |
    Version_equals of version |
    Version_newer_equals of version

type annotated_package_constraint =
    (package_constraint * string option)

let string_of_package_constraint_type = function
    | No_constraint -> "no_constraint"
    | Version_equals _ -> "version_equals"
    | Version_newer_equals _ -> "version_newer_equals"

(* This function constracts a short human readable string of a list of
 * package_constraints *)
let ui_string_of_annotated_constraints =

    let epsilon = "​ɛ​"
    in
    let append = function
        | a when a = epsilon -> (fun s -> s)
        | a -> (^) (a ^ " ")
    in
    List.fold_left
        (fun s (c, annotation) ->
            let suffix =
                match annotation with
                    | None -> ""
                    | Some str -> "(" ^ str ^ ")"
            in
            match c with
                | No_constraint -> s
                | Version_equals v ->
                    append s ("=" ^ string_of_version v ^ suffix)
                | Version_newer_equals v ->
                    append s (">=" ^ string_of_version v ^ suffix))
        epsilon

let merge_annotated_constraints cs1 cs2 =
    List.fold_left
        (fun a c ->
            match List.exists (fun o -> o = c) a with
                | true -> a
                | false -> c::a)
        cs1
        cs2

let cs_satisfied (v : version) (cs : package_constraint list) =
    List.exists
        (function
            | No_constraint ->
                false
            | Version_equals rv ->
                compare_version rv v <> 0
            | Version_newer_equals rv ->
                compare_version rv v > 0)
        cs
    |> not

(* Interprets strings like name=1.0.0 *)
let pkg_name_constraints_of_string str =
    let ne_ss =
        Str.split (Str.regexp ">=") str
    in
    match List.length ne_ss with
        | 2 ->
            let vs = List.nth ne_ss 1
            in
            (match version_of_string vs with
                | None -> print_endline ("Invalid version " ^ vs); None
                | Some v -> Some (List.hd ne_ss, [Version_newer_equals v]))
        | _ -> 

    let e_ss =
        Str.split (Str.regexp "=") str
    in
    match List.length e_ss with
        | 2 ->
            let vs = List.nth e_ss 1
            in
            (match version_of_string vs with
                | None -> print_endline ("Invalid version " ^ vs); None
                | Some v -> Some (List.hd e_ss, [Version_equals v]))
        | _ -> Some (str, [])

let unopt = function
    | None -> raise (Critical_error "unopt applied to None")
    | Some v -> v

let add_xml_descriptor = (^) "<?xml version=\"1.0\"?>\n"
let xml_to_string_with_desc s = (Xml.to_string_fmt s |> add_xml_descriptor) ^ "\n"

let compare_names (a : string) (b : string) =
    if a > b then 1 else (if a < b then -1 else 0)

let contains l e = List.exists (fun l -> l = e) l

(* Difference of sorted lists: l1 \ l2 *)
let sorted_difference cmp (l1 : 'a) (l2 : 'a) =
    List.filter
        (fun e -> not (contains l2 e))
        l1

(* Isolate files which are only in one of the given lists *)
let sorted_bidirectional_difference cmp (l1 : 'a) (l2 : 'a) =
    let rec work only_in_1 only_in_2 l1 l2 =
        match (l1, l2) with
            | ([], []) -> (only_in_1, only_in_2)
            | (l1::l1s, []) -> work (l1::only_in_1) only_in_2 l1s []
            | ([], l2::l2s) -> work only_in_1 (l2::only_in_2) [] l2s

            | (l1::l1s, l2::l2s) when cmp l1 l2 > 0 ->
                work only_in_1 (l2::only_in_2) (l1::l1s) l2s

            | (l1::l1s, l2::l2s) when cmp l1 l2 < 0 ->
                work (l1::only_in_1) only_in_2 l1s (l2::l2s)

            | (l1::l1s, l2::l2s) ->
                work only_in_1 only_in_2 l1s l2s
    in
    let (only_in_1, only_in_2) =
        work [] [] l1 l2
    in
    (List.rev only_in_1, List.rev only_in_2)

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

let sorted_merge cmp l1 l2 =
    let rec work buffer l1 l2 =
        match (l1, l2) with
            | ([], []) -> buffer
            | (l1::l1s, []) -> work (l1::buffer) l1s []
            | ([], l2::l2s) -> work (l2::buffer) l2s []
            | (l1::l1s, l2::l2s) ->
                if cmp l1 l2 > 0
                then work (l2::buffer) (l1::l1s) l2s
                else work (l1::buffer) l1s (l2::l2s)
    in
    work [] l1 l2 |> List.rev

(* Takes an absolute path *)
let form_target_path p =
    match !runtime_system with
        | Native_runtime -> p
        | Directory_runtime d -> d ^ p

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
    work (form_target_path "/") dirs

let rec rmdir_r dir =
    try
        Sys.readdir dir |> Array.to_list
        |> List.iter
            (fun de ->
                let de = dir ^ "/" ^ de
                in
                if Sys.is_directory de
                then (if rmdir_r de then () else failwith "")
                else Sys.remove de);
        Unix.rmdir dir;
        true
    with
        | Sys_error msg -> print_endline msg; false
        | Unix.Unix_error (c,_,_) -> Unix.error_message c |> print_endline; false
        | _ -> false

let print_failed () =
    print_endline (" [" ^ Terminal.red ^ "failed" ^ Terminal.normal ^ "]")

let print_ok () =
    print_endline (" [  " ^ Terminal.green ^ "OK" ^ Terminal.normal ^ "  ]")

let sha512sum_of_file_opt name =
    try
        let args = [|!program_sha512sum; name |]
        in
        let (ic,oc) =
            Unix.pipe ~cloexec:false ()
        in
        try
            let pid =
            Unix.create_process
                args.(0)
                args
                Unix.stdin
                oc
                Unix.stderr
            in
            let sha512sum =
                input_line (Unix.in_channel_of_descr ic)
            in
            if Unix.waitpid [] pid <> (pid, Unix.WEXITED 0)
            then
                (print_endline (!program_sha512sum ^ " failed."); None)
            else
                let sha512sum = String.split_on_char ' ' sha512sum |> List.hd
                in
                    Unix.close ic; Unix.close oc; Some sha512sum
        with e ->
            Unix.close ic; Unix.close oc; raise e
    with
        | Unix.Unix_error (c,_,_) ->
            print_endline ("Calculating the sha512 sum of file \"" ^
                name ^ "\" failed: " ^ Unix.error_message c ^ "."); None
        | _ -> print_endline ("Calculating the sha512 sum of file \"" ^
                name ^ "\" failed."); None

let array_is_empty a = (Array.length a = 0)

type file_status = Other_file | Directory | Non_existent | Read_error

let file_status n =
        try
            if (Unix.lstat n).st_kind = Unix.S_DIR
            then Directory
            else Other_file
        with
            | Unix.Unix_error (ENOENT,_,_) -> Non_existent
            | _ -> Read_error

let bool_of_option o = match o with None -> false | Some _ -> true

let create_or_clean_tmp_dir () =
    try
        match file_status Tpm_config.tmp_dir with
            | Non_existent -> create_tmp_dir ()
            | Directory -> (match rmdir_r Tpm_config.tmp_dir with
                | true -> create_tmp_dir ()
                | false -> false)
            | Other_file -> print_endline ("The supposed temporary directory \"" ^
                Tpm_config.tmp_dir ^ "\" exists already as other file.\""); false
            | Read_error -> print_endline ("Could not check if the temporary " ^
                "directory \"" ^ Tpm_config.tmp_dir ^ "\" exists already"); false
    with
        Sys_error msg -> print_endline ("Could not create or clean temprary " ^
            "directory \"" ^ Tpm_config.tmp_dir ^ "\": " ^ msg); false


type problem = Non_critical | Critical | No_problem

(* The following to functions set the respective problem state if the
 * 'second parameter' is true *)
let check_critical is = function
    | false -> is
    | true -> match is with
        | No_problem
        | Non_critical
        | Critical -> Critical


let check_non_critical is = function
    | false -> is
    | true -> match is with
        | No_problem
        | Non_critical -> Non_critical
        | Critical -> Critical

let all_packaging_scripts = [
    Tpm_config.configuresh_name;
    Tpm_config.unconfiguresh_name;
]

let hashtbl_keys ht =
    Hashtbl.fold
        (fun k _ l -> k::l)
        ht
        []

let hashtbl_kv_pairs ht =
    Hashtbl.fold
        (fun k v l -> (k, v)::l)
        ht
        []

let path_remove_double_slash str =
    let rec work pos lc dst =
        if String.length str > pos
        then
            let nlc = String.get str pos
            in
            match nlc with
                | '/' when lc = '/' -> work (pos + 1) nlc dst
                | _ -> work (pos + 1) nlc (dst ^ String.make 1 nlc)
        else
            dst
    in
    work 0 ' ' ""

(* Potentially raises a Unix_error *)
let run_program args =
    let pid =
        Unix.create_process
            args.(0)
            args
            Unix.stdin
            Unix.stdout
            Unix.stderr
    in
    Unix.waitpid [] pid

let print_string_flush str =
    print_string str;
    flush stdout

let basename path =
    match String.rindex_opt path '/' with
        | None -> path
        | Some pos ->
            let slen = String.length path
            in
            String.sub path (pos + 1) (slen - (pos + 1))

(* Like coreutil's install for files
 * Raises a Unix_error potentially *)
let install_files (uid, gid) mode paths destination =
    let cp_file src =
        (* See https://ocaml.github.io/ocamlunix/ocamlunix.html, section 2.9 *)
        let buffer = Bytes.create Tpm_config.cp_buffer_size
        in
        let dst = destination ^ "/" ^ basename src
        in
        let in_fd = Unix.openfile src [O_RDONLY] 0
        in
        let out_fd =
            Unix.openfile
                dst
                [O_WRONLY; O_CREAT; O_TRUNC]
                mode
        in
        let rec loop () =
            match Unix.read in_fd buffer 0 Tpm_config.cp_buffer_size with
                | 0 -> ()
                | l -> ignore (
                    Unix.write out_fd buffer 0 l);
                    loop ()
        in
        loop ();
        Unix.close in_fd;
        Unix.close out_fd;
        Unix.chown dst uid gid
    in
    List.iter cp_file paths

module Perf_hash = struct
    type hf = (int * (string, int) Hashtbl.t)
    
    let create_empty () = (0, Hashtbl.create ~random:true 100)

    let map (hf : hf) x =
        let (num, ht) = hf
        in
        match Hashtbl.find_opt ht x with
            | Some y -> (hf, y)
            | None ->
                Hashtbl.add ht x num;
                ((num + 1, ht), num)
end

let rec rassoc_opt b = function
    | [] -> None
    | (la, lb)::ls ->
        if lb = b then Some la else rassoc_opt b ls

(* Traverses the files in a directory tree in DFS manner *)
let fold_directory_tree f acc name =
    let rec work f acc name = match acc with
        | None -> None
        | Some acc -> match file_status name with
            | Other_file ->
                Some (f acc name)
            | Directory ->
                Sys.readdir name
                |> Array.to_list
                |> List.map (fun n -> name ^ "/" ^ n)
                |> List.fold_left
                    (work f)
                    (Some acc)
            | Non_existent
            | Read_error -> failwith ("Could not read " ^ name)
    in
    try
        work f (Some acc) name
    with
        | Sys_error msg ->
            print_endline ("fold_directory_tree: " ^ msg);
            None
        | Failure msg ->
            print_endline ("fold_directory_tree: " ^ msg);
            None
        | _ -> None
