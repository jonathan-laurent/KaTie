val eval :
  Model.t -> Query.query -> Format.formatter -> string -> unit

val eval_queries :
  ?skip_init_events:bool ->
  Model.t -> (Query.query * Format.formatter) list -> string -> unit