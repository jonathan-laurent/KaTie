(*****************************************************************************)
(* Safe Replay                                                               *)
(*****************************************************************************)

(* This module is a wrapper over [Replay] that assigns unique ids to all
   agents in such a way that no two agents can ever share the same id,
   even if their existence never overlaps in time. Indeed, for
   performance reasons, KaSim reuses ids of deleted agents when creating
   new ones. This is unsuitable for reasoning about causality, where
   tracking agent identity over time is important. *)

(* There is currently a bug in the traces produced by KaSim where the
   same identifier can sometimes refer to multiples agents _within_ the
   same trace step. In addition, executing actions in the order they are
   presented can lead to attempting to create an agent that is already
   in the mixture. Thus, our procedure for translating trace steps is
   somewhat more convoluted than it should be. In particular, we reorder
   all action lists to put deletion firsts. We also translate actions
   sequentially, potentially updating the mapping between simulator ids
   (simids) and unique ids (uids) between each action. *)

(* We have an imperative interface since Replay.state is not persistent
   anyway (the graph gets updated in place). In this file, [simid]
   denotes simulator ids (agent ids as used in the original trace files)
   and [uid] denotes unique agent ids, as manipulated by this tool. *)
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

let s2u state simid =
  (* We avoid raising `Not_found` since this exception may be caught
     accidentally by some other code in case of an internal error. *)
  try Hashtbl.find state.simid_to_uid simid with Not_found -> assert false

(* Functions from the [Graph] module raise this exception when called
   with an id that does not map to an existing agent. *)
exception Inexisting_agent

let u2s state uid =
  try Hashtbl.find state.uid_to_simid uid
  with Not_found -> raise Inexisting_agent

let rename_agent f (ag_id, ag_ty) = (f ag_id, ag_ty)

let s2u_ag state simid_ag = rename_agent (s2u state) simid_ag

let u2s_ag state uid_ag = rename_agent (u2s state) uid_ag

let s2u_site state (ag, s) = (s2u_ag state ag, s)

let update_action state action =
  let open Instantiation in
  let tr = subst_map_agent_in_concrete_action (s2u state) in
  match action with
  | Instantiation.Create (ag, _) ->
      let simid = Agent.id ag in
      create_agent state simid ; tr action
  | Remove ag ->
      let simid = Agent.id ag in
      let action = tr action in
      delete_agent state simid ; action
  | _ ->
      tr action

let update_actions state actions =
  (* Removals actions are placed and processed first *)
  let is_removal = function Instantiation.Remove _ -> true | _ -> false in
  let removals, others = Utils.partition_tailrec is_removal actions in
  let removals = Utils.map_tailrec (update_action state) removals in
  let others = Utils.map_tailrec (update_action state) others in
  removals @ others

let update_tests state tests =
  Utils.map_tailrec
    (Instantiation.subst_map_agent_in_concrete_test (s2u state))
    tests

let update_event state e =
  let open Instantiation in
  (* Tests and side effects are translated BEFORE we process the actions
     and update the id correspondence table *)
  let tests = Utils.map_tailrec (update_tests state) e.tests in
  let side_effects_src =
    Utils.map_tailrec
      (subst_map_agent_in_concrete_side_effect (s2u state))
      e.side_effects_src
  in
  let connectivity_tests = update_tests state e.connectivity_tests in
  let side_effects_dst =
    Utils.map_tailrec (s2u_site state) e.side_effects_dst
  in
  (* Processing actions updates the mapping imperatively *)
  let actions = update_actions state e.actions in
  {tests; side_effects_src; side_effects_dst; connectivity_tests; actions}

let update_step st =
  let open Trace in
  function
  | Subs (i, j) ->
      Subs (s2u st i, s2u st j)
  | Rule (r, e, info) ->
      Rule (r, update_event st e, info)
  | Pert (s, e, info) ->
      Pert (s, update_event st e, info)
  | Init actions ->
      Init (update_actions st actions)
  | Obs (s, tests, info) ->
      Obs (s, Utils.map_tailrec (update_tests st) tests, info)
  | Dummy s ->
      Dummy s

let replay_and_translate_step sigs st raw_step =
  let state, _ = Replay.do_step sigs st.state raw_step in
  st.state <- state ;
  update_step st raw_step

let rename_agents_in_user_graph_cc f cc =
  let open User_graph in
  let rename_node n = {n with node_id= (Option.map f) n.node_id} in
  Array.map (Array.map (Option.map rename_node)) cc

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

  let species ~debugMode sigs uid g =
    Edges.species ~debugMode sigs (u2s g uid) g.state.graph
    |> rename_agents_in_user_graph_cc (s2u g)
end

(* TODO: what is the integer associated to each connected component? *)
let snapshot ~raw model g =
  let sigs = Model.signatures model in
  let snap = Edges.build_snapshot ~raw sigs g.state.graph in
  let ug = Snapshot.export ~raw:true ~debugMode:true sigs snap in
  Utils.map_tailrec
    (fun (i, cc) -> (i, rename_agents_in_user_graph_cc (s2u g) cc))
    ug

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
