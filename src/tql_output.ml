let snapshot_prefix = ref "snapshot."
let snapshot_suffix = ref ".json"
let snapshot_counter = ref 0

let set_snapshots_name_format fmt =
  snapshot_counter := 0 ;
  let parts = String.split_on_char '%' fmt in
  match parts with
  | [pre ; suf] ->
    begin
      snapshot_prefix := pre ;
      snapshot_suffix := suf
    end
  | _ -> ()

let new_snapshot_file () =
  let f = Format.asprintf "%s%d%s" 
    !snapshot_prefix !snapshot_counter !snapshot_suffix in
  incr snapshot_counter ;
  f