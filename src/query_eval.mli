val eval :
  ?uuid:int -> Model.t -> Query.query -> Format.formatter -> string -> unit

val eval_queries :
     ?skip_init_events:bool
  -> ?uuid:int
  -> Model.t
  -> (Query.query * Format.formatter) list
  -> string
  -> unit

(* The following declarations are included to avoid warnings of the
   kind: "Unused function pp_env". *)

type env [@@deriving show, yojson_of]

type complete_matching [@@deriving show, yojson_of]
