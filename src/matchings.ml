(*****************************************************************************)
(* Trace and event matchings                                                 *)
(*****************************************************************************)


(*  For a given event, a valuation maps every pinned-down agent
    to a concrete agent. *)

module Ag_valuation =
struct
    type t = int list
    let compare v v' =
        assert (List.length v = List.length v') ;
        compare v v'
end

module ValMap = Map.Make (Ag_valuation)

type ev_matching = {
    specific : ev_matching_specific_part ;
    common   : ev_matching_common_part ;
}

and ev_matching_common_part = {
    ev_id_in_trace : int ;
    ev_id_in_query : int ;
    ev_time : float ;
    indexing_ag_matchings : Ag_valuation.t ;
}

and ev_matching_specific_part = {
    new_ag_matchings : Ag_valuation.t ;
    recorded_measures : (Query.value option) Utils.IntMap.t ;
}

type ev_matchings = {
    common_to_all : ev_matching_common_part ;
    matchings : ev_matching list ;
}

type complete_matching = {
    cm_agents : int array ;
    cm_events : ev_matching array ;
    cm_last_matched_step_id : int
}