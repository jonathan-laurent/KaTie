open Aliases

type state_measure =
  | Int_state of (local_agent_id * site_id)
  | Component of local_agent_id
  | Print_cc of site_id
  | Snapshot
[@@deriving show, yojson_of]

type event_measure = Time | Rule | Debug_event | Init_event
[@@deriving show, yojson_of]

type state_measure_time = Before | After [@@deriving show, yojson_of]

type t =
  | State_measure of state_measure_time * state_measure
  | Event_measure of event_measure
[@@deriving show, yojson_of]

val take_measure :
     header:Trace_header.t
  -> (local_agent_id -> global_agent_id)
  -> Streaming.window
  -> t
  -> Value.t
