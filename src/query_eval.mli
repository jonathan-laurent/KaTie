val eval :
  ?uuid:int -> Model.t -> Query.query -> Format.formatter -> string -> unit

val eval_queries :
     ?skip_init_events:bool
  -> ?uuid:int
  -> Model.t
  -> (Query.query * Format.formatter) list
  -> string
  -> unit

type env [@@deriving show, yojson_of]
