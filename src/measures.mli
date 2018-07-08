val take_measures :
  Model.t ->
  Query.event ->
  int array ->
  Streaming.window ->
  (int -> Query.value option -> unit) ->
  unit