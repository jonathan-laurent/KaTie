val snapshot_prefix : string ref
val snapshot_suffix : string ref
val snapshot_counter : int ref
val snapshots_native_format : bool ref
val output_directory : string ref

val set_output_directory : string -> unit
val file : string -> string
val set_snapshots_name_format : string -> unit
val new_snapshot_file : unit -> string