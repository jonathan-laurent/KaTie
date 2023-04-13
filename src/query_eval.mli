val eval_batch :
  trace_file:string -> (Query.query * Format.formatter) list -> unit

(* The following declarations are included to avoid warnings of the
   kind: "Unused function pp_env". *)

type env [@@deriving show, yojson_of]

type complete_matching [@@deriving show, yojson_of]
