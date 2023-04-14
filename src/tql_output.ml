let debug_level = ref 1

let snapshot_prefix = ref "snapshot."

let snapshot_suffix = ref ".json"

let snapshot_counter = ref 0

let snapshots_native_format = ref false

let output_directory = ref "."

let check_dir_exists dir =
  if not (Sys.file_exists dir && Sys.is_directory dir) then
    if Sys.command ("mkdir '" ^ dir ^ "'") <> 0 then
      Tql_error.(fail (Sys_error ("Could not create directory '" ^ dir ^ "'.")))

let set_output_directory dir =
  let dir =
    if dir = "" then "."
    else if String.get dir (String.length dir - 1) = '/' then
      String.sub dir 0 (String.length dir - 1)
    else dir
  in
  output_directory := dir

let file filename =
  check_dir_exists !output_directory ;
  !output_directory ^ "/" ^ filename

let with_file filename f =
  let oc = open_out (file filename) in
  let fmt = Format.formatter_of_out_channel oc in
  f fmt ; close_out oc

let debug_json ?(level = 1) filename yojson_of obj =
  if !debug_level >= level then
    with_file filename (fun fmt ->
        Format.fprintf fmt "%a@.]"
          (Yojson.Safe.pretty_print ~std:false)
          (yojson_of obj) )

let set_snapshots_name_format fmt =
  snapshot_counter := 0 ;
  let parts = String.split_on_char '%' fmt in
  match parts with
  | [pre; suf] ->
      snapshot_prefix := pre ;
      snapshot_suffix := suf
  | _ ->
      Tql_error.failwith "Illegal snapshots name format."

let new_snapshot_file () =
  let f =
    Format.asprintf "%s%d%s" !snapshot_prefix !snapshot_counter !snapshot_suffix
  in
  incr snapshot_counter ; file f
