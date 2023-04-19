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

let event st = st.state.event

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

let rename_agent f (ag_id, ag_ty) = (f ag_id, ag_ty)

let s2u_ag state simid_ag = rename_agent (s2u state) simid_ag

let u2s_ag state uid_ag = rename_agent (u2s state) uid_ag

let s2u_site state (ag, s) = (s2u_ag state ag, s)

module Graph = struct
  type t = state

  let is_agent ag g = Edges.is_agent (u2s_ag g ag) g.state.graph

  let is_agent_id uid g = Edges.is_agent_id (u2s g uid) g.state.graph

  let get_sort uid g = Edges.get_sort (u2s g uid) g.state.graph

  let is_free uid site g = Edges.is_free (u2s g uid) site g.state.graph

  let get_internal uid site g =
    Edges.get_internal (u2s g uid) site g.state.graph

  let link_exists uid1 site1 uid2 site2 g =
    Edges.link_exists (u2s g uid1) site1 (u2s g uid2) site2 g.state.graph

  let link_destination ag site g =
    Edges.link_destination (u2s g ag) site g.state.graph
    |> Option.map (s2u_site g)

  (* TODO: this may show simulation ids *)
  let species ~debugMode sigs uid g =
    Edges.species ~debugMode sigs (u2s g uid) g.state.graph

  (* TODO: this shows simulation ids *)
  let build_snapshot ~raw sigs g = Edges.build_snapshot ~raw sigs g.state.graph
end

let simid_connected_component simid state =
  match state.Replay.connected_components with
  | None ->
      Log.failwith "No connected component information available."
  | Some ccs -> (
      let cc_id = Edges.get_connected_component simid state.Replay.graph in
      match
        Option.bind cc_id (fun cc_id -> Mods.IntMap.find_option cc_id ccs)
      with
      | None ->
          Log.(
            failwith
              (fmt "Impossible to find the cc of agent with simid %d." simid)
              ~details:
                [ fmt "`cc_id` is equal to None: %b." (cc_id = None)
                ; fmt "`ag_id` is a valid ID: %b."
                    (Edges.is_agent_id simid state.Replay.graph) ] )
      | Some cc ->
          cc )

(* Same than [simid_connected_component] but works with uids *)
let connected_component uid st =
  let simid = u2s st uid in
  let scc = simid_connected_component simid st.state in
  let open Agent.SetMap.Set in
  fold (fun sa ucc -> add (s2u_ag st sa) ucc) scc empty

(** How should we translate the step? every time an action is executed,

    Delete 8
    Delete 9

    Side effects: src in Rule and Pert.

    Some tests are performed before removing...

    Translate side_effects before.

    reorder in an advanced way...

    what about renaming step on the fly?

    if there are several agent 8 in step, we are done...

*)
