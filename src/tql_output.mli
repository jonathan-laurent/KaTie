val debug_mode : bool ref

val snapshot_prefix : string ref

val snapshot_suffix : string ref

val snapshot_counter : int ref

val snapshots_native_format : bool ref

val set_output_directory : string -> unit

val file : string -> string

val with_file : string -> (Format.formatter -> unit) -> unit

val debug_json : string -> ('a -> Yojson.Safe.t) -> 'a -> unit

val set_snapshots_name_format : string -> unit

val new_snapshot_file : unit -> string
