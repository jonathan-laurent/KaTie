(*****************************************************************************)
(* Safe Replay                                                               *)
(*****************************************************************************)

include Replay

(* We have an imperative interface since Replay.state is not persistent
   anyway (the graph gets updated in place). *)

type state =
  { mutable state: Replay.state
  ; uid_to_simid: (int, int) Hashtbl.t
  ; simid_to_uid: (int, int) Hashtbl.t
  ; mutable fresh_uid: int }

let graph st = st

let time st = st.state.time

let init_hashtbl_size = 1_000_000

let init_state ~with_connected_components =
  { state= Replay.init_state ~with_connected_components
  ; uid_to_simid= Hashtbl.create init_hashtbl_size
  ; simid_to_uid= Hashtbl.create init_hashtbl_size
  ; fresh_uid= 0 }

let fresh state =
  let id = state.fresh_uid in
  state.fresh_uid <- state.fresh_uid + 1 ;
  id

let create_agent state simid =
  let uid = fresh state in
  Hashtbl.add state.uid_to_simid uid simid ;
  Hashtbl.add state.simid_to_uid simid uid

let delete_agent state simid =
  let uid = Hashtbl.find state.simid_to_uid simid in
  Hashtbl.remove state.uid_to_simid uid ;
  Hashtbl.remove state.simid_to_uid simid

let update_uids_for_action state = function
  | Instantiation.Create (ag, _) ->
      let simid = Agent.id ag in
      create_agent state simid
  | Remove ag ->
      let simid = Agent.id ag in
      delete_agent state simid
  | _ ->
      ()

let update_uids_for_step state step =
  let actions, _ = Trace.actions_of_step step in
  let is_removal = function Instantiation.Remove _ -> true | _ -> false in
  let removals, others = List.partition is_removal actions in
  List.iter (update_uids_for_action state) removals ;
  List.iter (update_uids_for_action state) others

let do_step sigs st step =
  let state, _ = Replay.do_step sigs st.state step in
  st.state <- state ;
  update_uids_for_step st step

let s2u state simid = Hashtbl.find state.simid_to_uid simid

let u2s state uid = Hashtbl.find state.uid_to_simid uid

module Edges = struct
  type t = state

  let is_agent_id uid g = Edges.is_agent_id (u2s g uid) g.state.graph
end
