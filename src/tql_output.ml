let snapshot_prefix = ref "snapshot."
let snapshot_suffix = ref ".json"
let snapshot_counter = ref 0
let snapshots_native_format = ref false
let output_directory = ref "."

let check_dir_exists dir =
  if not (Sys.file_exists dir && Sys.is_directory dir) then
  begin
    if Sys.command ("mkdir '" ^ dir ^ "'") <> 0 then
      Tql_error.(fail (Sys_error ("Could not create directory '" ^ dir ^ "'.")))
  end

let set_output_directory dir =
  let dir =
    if dir = "" then "."
    else if String.get dir (String.length dir - 1) = '/' then
      String.sub dir 0 (String.length dir - 1)
    else dir in
  output_directory := dir

let file filename =
  check_dir_exists !output_directory ;
  !output_directory ^ "/" ^ filename

let set_snapshots_name_format fmt =
  snapshot_counter := 0 ;
  let parts = String.split_on_char '%' fmt in
  match parts with
  | [pre ; suf] ->
    begin
      snapshot_prefix := pre ;
      snapshot_suffix := suf
    end
  | _ -> Tql_error.failwith "Illegal snapshots name format."

let new_snapshot_file () =
  let f = Format.asprintf "%s%d%s"
    !snapshot_prefix !snapshot_counter !snapshot_suffix in
  incr snapshot_counter ;
  file f