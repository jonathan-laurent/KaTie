(*****************************************************************************)
(* Output Files                                                              *)
(*****************************************************************************)

(* KaTie outputs the following directory structure:
   - out
      - errors.json
      - debug
          - trace-raw.json, queries-ast.json, ...
      - results
          - query_1.csv, ..., query_n.csv
      - snapshots
          - snapshot.1.json, ..., snapshot.k.json
*)

(* Constants *)

let results_dir = "results"

let snapshots_dir = "snapshots"

let debug_dir = "debug"

(* Configuration *)

let debug_level = ref 1

let snapshot_prefix = ref "snapshot."

let snapshot_suffix = ref ".json"

let snapshots_native_format = ref false

let output_directory = ref Filename.current_dir_name

(* Functions *)

type kind = [`Result | `Snapshot | `Debug]

let snapshot_counter = ref 0

let rec mkdir_p dir =
  if dir <> Filename.current_dir_name then (
    mkdir_p (Filename.dirname dir) ;
    if Sys.file_exists dir && not (Sys.is_directory dir) then
      Error.failwith
        ( "This directory cannot be created since a file with the same name \
           already exists: " ^ dir ) ;
    if not (Sys.file_exists dir) then Sys.mkdir dir 0O777 )

let ensure_containing_dir_exists filename = mkdir_p (Filename.dirname filename)

let set_output_directory dir =
  let dir =
    if dir = "" then "."
    else if String.get dir (String.length dir - 1) = '/' then
      String.sub dir 0 (String.length dir - 1)
    else dir
  in
  output_directory := dir

let file ?kind filename =
  assert (Filename.is_implicit filename) ;
  let filename =
    match kind with
    | None ->
        filename
    | Some `Result ->
        Filename.concat results_dir filename
    | Some `Snapshot ->
        Filename.concat snapshots_dir filename
    | Some `Debug ->
        Filename.concat debug_dir filename
  in
  let filename = Filename.concat !output_directory filename in
  ensure_containing_dir_exists filename ;
  filename

let with_file ?kind filename f =
  let oc = open_out (file ?kind filename) in
  let fmt = Format.formatter_of_out_channel oc in
  f fmt ; close_out oc

let write_json ?kind filename json =
  with_file ?kind filename (fun fmt ->
      Format.fprintf fmt "%a@.]" (Yojson.Safe.pretty_print ~std:false) json )

let debug_json ?(level = 1) ?show_message filename make_json =
  if !debug_level >= level then
    let run f =
      let show_message = Option.value show_message ~default:(level > 1) in
      if show_message then Terminal.task ("Producing debug info: " ^ filename) f
      else f ()
    in
    run (fun () ->
        with_file ~kind:`Debug filename (fun fmt ->
            Format.fprintf fmt "%a@.]"
              (Yojson.Safe.pretty_print ~std:false)
              (make_json ()) ) )

let set_snapshots_name_format fmt =
  snapshot_counter := 0 ;
  let parts = String.split_on_char '%' fmt in
  match parts with
  | [pre; suf] ->
      snapshot_prefix := pre ;
      snapshot_suffix := suf
  | _ ->
      Error.failwith "Illegal snapshots name format."

let new_snapshot_file () =
  let f =
    Format.asprintf "%s%d%s" !snapshot_prefix !snapshot_counter !snapshot_suffix
  in
  incr snapshot_counter ; file ~kind:`Snapshot f
