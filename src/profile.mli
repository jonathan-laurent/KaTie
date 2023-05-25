val profiling_enabled : bool ref

val with_elapsed_time : (unit -> 'a) -> 'a * float

val record : ?query:string -> string -> (unit -> 'a) -> 'a

val dump_json : unit -> Yojson.Safe.t
