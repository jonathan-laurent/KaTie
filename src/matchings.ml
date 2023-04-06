(*****************************************************************************)
(* Trace and event matchings                                                 *)
(*****************************************************************************)

(* For a given event, a valuation maps every pinned-down agent
   to a concrete agent. *)

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
  ; indexing_ag_matchings: Ag_valuation.t (* Maps some agents to *) }

type ev_matching_specific_part =
  { new_ag_matchings: Ag_valuation.t
  ; recorded_measures: Query.value option Utils.IntMap.t }

type ev_matching =
  {specific: ev_matching_specific_part; common: ev_matching_common_part}

type ev_matchings =
  {common_to_all: ev_matching_common_part; matchings: ev_matching list}

(* This is a full matching. *)
type complete_matching =
  { cm_agents: global_agent_id array (* local_agent_id -> global_agent_id *)
  ; cm_events: ev_matching array (* local_event_id -> ev_matching *)
  ; cm_last_matched_step_id: int
        (* What is the last `global_event_id` involved in the matching? This
           information is useful to determine when to perform the action. *) }
