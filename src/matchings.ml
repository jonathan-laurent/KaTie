(*****************************************************************************)
(* Trace and event matchings                                                 *)
(*****************************************************************************)

open Aliases

module Ag_valuation = struct
  (* local_agent_id -> global_agent_id *)
  type t = int list

  let compare v v' =
    assert (List.length v = List.length v') ;
    compare v v'
end

module ValMap = Map.Make (Ag_valuation)

type ev_matching_common_part =
  { ev_id_in_trace: global_event_id
  ; ev_id_in_query: local_event_id
  ; ev_time: float
  ; indexing_ag_matchings: Ag_valuation.t
        (* Maps [local_agent_id]s from [event.already_constrained_agents]
           to [global_agent_id] *) }

type ev_matching_specific_part =
  { new_ag_matchings: Ag_valuation.t
        (* Maps [local_agent_id]s from [event.captured_agents] to
           [global_agent_id] *)
  ; recorded_measures: Query.value option Utils.IntMap.t
        (* This is only used during the second pass. TODO: should we use
           an array instead? *) }

type ev_matching =
  {specific: ev_matching_specific_part; common: ev_matching_common_part}

type ev_matchings =
  {common_to_all: ev_matching_common_part; matchings: ev_matching list}

(* This is a full matching. *)
type complete_matching =
  { cm_agents: global_agent_id array (* local_agent_id -> global_agent_id *)
  ; cm_events: ev_matching array
  ; cm_last_matched_step_id: int
        (* What is the last `global_event_id` involved in the matching? This
           information is useful to determine when to perform the action. *) }
