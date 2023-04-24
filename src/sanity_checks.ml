(*****************************************************************************)
(* Sanity checks                                                             *)
(*****************************************************************************)

(* Some sanity checks to be performed on a query before it is executed
   to avoid time-wasting runtime errors. *)

(* Checking types *)

let dummy_eval_measure =
  let open Measure in
  let open Value in
  function
  | Event_measure Time ->
      VFloat 0.0
  | Event_measure Rule ->
      VString ""
  | Event_measure Debug_event ->
      VString ""
  | State_measure (_, Int_state _) ->
      VString ""
  | State_measure (_, Snapshot) ->
      VString ""
  | State_measure (_, Print_cc _) ->
      VString ""
  | State_measure (_, Component _) ->
      VAgentSet AgentSet.empty

let dummy_eval_expr q e =
  let read_measure ev_lid m_id =
    q.Query.trace_pattern.events.(ev_lid).measures.(m_id).measure
    |> dummy_eval_measure
  in
  let read_agent_id _ = 0 in
  let read_event_id _ = 0 in
  Expr.eval_expr ~read_measure ~read_agent_id ~read_event_id e

let check_types q e = ignore (dummy_eval_expr q e)

let rec check_action_types q = function
  | Query.Print e ->
      check_types q e
  | If (c, a) ->
      check_types q c ; check_action_types q a

let check_query_types q = check_action_types q q.Query.action

let check_every_clause q =
  match q.Query.every_clause with
  | Some dt when dt <= 0. ->
      Error.failwith "The 'every' clause must be associated a positive number"
  | _ ->
      ()

(* Check that events are rooted *)

(* constrained is a list of constrained [local_id] *)
let check_rooted ~ev_name ?(constrained = []) (pat : Query.pattern) =
  let open Query in
  let rooted = Array.map (fun _ -> false) pat.agents in
  let set_rooted i = rooted.(i) <- true in
  Utils.IntMap.iter
    (fun lid patid -> if List.mem lid constrained then set_rooted patid)
    pat.agent_constraints ;
  List.iter
    (function
      | Create i ->
          set_rooted i
      | Destroy i ->
          set_rooted i
      | Mod_int_state ((i, _), _) ->
          set_rooted i
      | Mod_lnk_state ((i, _), Bound_to (j, _)) ->
          set_rooted i ; set_rooted j
      | Mod_lnk_state ((i, _), (Free | Bound_to_type _ | Bound_to_any)) ->
          set_rooted i )
    pat.mods ;
  let converged = ref false in
  while not !converged do
    converged := true ;
    List.iter
      (function
        | Lnk_state_is ((i, _), Bound_to (j, _))
          when rooted.(i) && not rooted.(j) ->
            set_rooted j ;
            converged := false
        | _ ->
            () )
      pat.tests
  done ;
  Array.iter
    (fun r ->
      if not r then
        Error.failwith
          ( "There is a non-rooted agent in the pattern defining event "
          ^ ev_name ) )
    rooted

let check_patterns_rooted_in_ev e =
  let open Query in
  let ev_name = Option.value e.event_name ~default:"?" in
  match (defining_pattern e, e.event_pattern) with
  | None, None ->
      assert false
  | Some p, None | None, Some p ->
      check_rooted ~ev_name p.pattern
  | Some def_p, Some main_p ->
      check_rooted ~ev_name def_p.pattern ;
      let constrained =
        (* Compute [local_agent_id]s constrained by [def_p] *)
        Utils.IntMap.bindings def_p.pattern.agent_constraints |> List.map fst
      in
      check_rooted ~ev_name ~constrained main_p.pattern

let check_patterns_rooted q =
  Array.iter check_patterns_rooted_in_ev q.Query.trace_pattern.events

(* Main checking function *)

let run q = check_query_types q ; check_every_clause q ; check_patterns_rooted q
