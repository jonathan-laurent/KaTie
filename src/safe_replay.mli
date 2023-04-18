type state

val init_state : with_connected_components:bool -> state

val do_step : Signature.s -> state -> Trace.step -> unit

module Edges : sig
  type t

  val is_agent_id : int -> t -> bool
end

val graph : state -> Edges.t

val time : state -> float
