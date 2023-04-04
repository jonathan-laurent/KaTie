type measures_provider = Query.event_id * Query.measure_id -> Query.value option

type agent_ids_provider = int -> int option

val eval_expr :
  measures_provider -> agent_ids_provider -> 'a Query.expr -> 'a option

val eval_expr_to_value :
  measures_provider -> agent_ids_provider -> 'a Query.expr -> Query.value option

val print_value : Format.formatter -> Query.value -> unit
