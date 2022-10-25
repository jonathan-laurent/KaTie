val take_measure:
  ?uuid:int -> Model.t -> int array -> Streaming.window ->
  Query.measure -> Query.value option
