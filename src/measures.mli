val take_measures :
  ?uuid:int ->
  Model.t ->
  Query.event ->
  int array ->
  Streaming.window ->
  (int -> Query.value option -> unit) ->
  unit
