type state

val init_state : with_connected_components:bool -> state

val replay_and_translate_step : Signature.s -> state -> Trace.step -> Trace.step

exception Inexisting_agent

module Graph : sig
  type t

  val is_agent : Agent.t -> t -> bool

  val is_agent_id : int -> t -> bool

  val get_sort : int -> t -> int

  val get_internal : int -> int -> t -> int

  val is_free : int -> int -> t -> bool

  val link_exists : int -> int -> int -> int -> t -> bool

  val link_destination : int -> int -> t -> (Agent.t * int) option

  val species :
    debugMode:bool -> Signature.s -> int -> t -> User_graph.connected_component

  val build_snapshot : raw:bool -> Signature.s -> t -> Snapshot.t
end

val graph : state -> Graph.t

val time : state -> float

val event : state -> int

val connected_component : int -> state -> Agent.SetMap.Set.t
