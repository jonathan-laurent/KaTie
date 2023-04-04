(*****************************************************************************)
(* Event matcher                                                             *)
(*****************************************************************************)

open Matchings
open Query
open Utils
open Streaming

(*****************************************************************************)
(* Simple utilities                                                          *)
(*****************************************************************************)

let extract_tests_actions = function
  | Trace.Subs _ ->
      ([], [])
  | Trace.Rule (_, ev, _) ->
      (ev.Instantiation.tests, ev.Instantiation.actions)
  | Trace.Pert (_, ev, _) ->
      (ev.Instantiation.tests, ev.Instantiation.actions)
  | Trace.Init acts ->
      ([], acts)
  | Trace.Obs (_, tests, _) ->
      (tests, [])
  | Trace.Dummy _ ->
      ([], [])

let meaningful_step = function
  | Trace.Subs _ | Trace.Dummy _ ->
      false
  | _ ->
      true

let satisfy_rule_constraint cs _state step =
  match cs with
  | None ->
      meaningful_step step
  | Some Init ->
      false (* TODO *)
  | Some End_of_trace ->
      false (* TODO *)
  | Some (Obs s) -> (
    match step with Trace.Obs (s', _, _) -> s = s' | _ -> false )
  | Some (Rule rs) -> (
    match step with Trace.Rule (r, _, _) -> List.mem r rs | _ -> false )

(* For each pair in the result, its symmetric is also in it *)
let pattern_tested_links pat =
  pat.main_pattern.tests
  |> List.concat_map (function
       | Lnk_state_is (s, Bound_to s') ->
           [(s, s'); (s', s)]
       | _ ->
           [] )

let pattern_created_links pat =
  pat.main_pattern.mods
  |> List.concat_map (function
       | Mod_lnk_state (s, Bound_to s') ->
           [(s, s'); (s', s)]
       | _ ->
           [] )

let site_ag_id ((id, _), _) = id

let psite_ag_id (id, _) = id

let agent_ty ag = snd ag

let agent_id ag = fst ag

let p_agent_ty pat ag_pid = pat.main_pattern.agents.(ag_pid).agent_kind

let rec fixpoint eq f x =
  let fx = f x in
  if eq x fx then x else fixpoint eq f fx

(*****************************************************************************)
(* Main procedures                                                           *)
(*****************************************************************************)

(* [(pattern id, global id)] *)
type partial_agents_matching = PAM of (int * int) list

exception No_match

(* Try to match the agents of [pat] in [w] by only looking at
   modifications and connectivity. Only uses w.step (not the mixture) *)

let match_agents_in_pattern (pat : Query.event_pattern) (w : Streaming.window) :
    int array option =
  if satisfy_rule_constraint pat.rule_constraint w.state w.step then
    try
      let _tests, actions = extract_tests_actions w.step in
      let compatible_agents ag_pid ag = p_agent_ty pat ag_pid = agent_ty ag in
      let compatible_sites (ag_pid, s) (ag, s') =
        compatible_agents ag_pid ag && s = s'
      in
      let site_has_type (kind, s) ((_, kind'), s') = kind = kind' && s = s' in
      (* Take a pattern action and a step action and return all partial agent matchings
         that are induced by this pair. For example, the binding action in the
         event pattern {s1:S(x[./1]), s2:S(x[./1])}, when matching with a concrete
         binding action involving agents a1 and a2 in the trace, induces two
         matchings {s1->a1, s2->a2} and {s1->a2, s2->a1}. *)
      let match_actions pat_action step_action =
        match (pat_action, step_action) with
        | Create ag_pid, Instantiation.Create (ag, _)
        | Destroy ag_pid, Instantiation.Remove ag ->
            if compatible_agents ag_pid ag then [PAM [(ag_pid, agent_id ag)]]
            else []
        | Mod_int_state (site, st), Instantiation.Mod_internal (site', st') ->
            if compatible_sites site site' && st = st' then
              [PAM [(psite_ag_id site, site_ag_id site')]]
            else []
        | Mod_lnk_state (site, Free), Instantiation.Free site' ->
            if compatible_sites site site' then
              [PAM [(psite_ag_id site, site_ag_id site')]]
            else []
        | Mod_lnk_state (s1, Bound_to s2), Instantiation.Bind (s1', s2')
        | Mod_lnk_state (s1, Bound_to s2), Instantiation.Bind_to (s1', s2') ->
            let aux s1 s2 s1' s2' =
              if compatible_sites s1 s1' && compatible_sites s2 s2' then
                [ PAM
                    [ (psite_ag_id s1, site_ag_id s1')
                    ; (psite_ag_id s2, site_ag_id s2') ] ]
              else []
            in
            aux s1 s2 s1' s2' @ aux s1 s2 s2' s1'
        | Mod_lnk_state (s, Bound_to_any), Instantiation.Bind (s1, s2)
        | Mod_lnk_state (s, Bound_to_any), Instantiation.Bind_to (s1, s2) ->
            let aux s s' =
              if compatible_sites s s' then
                [PAM [(psite_ag_id s, site_ag_id s')]]
              else []
            in
            aux s s1 @ aux s s2
        | Mod_lnk_state (s, Bound_to_type t), Instantiation.Bind (s1, s2)
        | Mod_lnk_state (s, Bound_to_type t), Instantiation.Bind_to (s1, s2) ->
            let aux s s1 s2 =
              if compatible_sites s s1 && site_has_type t s2 then
                [PAM [(psite_ag_id s, site_ag_id s1)]]
              else []
            in
            aux s s1 s2 @ aux s s2 s1
        | _ ->
            []
      in
      (* Each action from the pattern must induce only one partial matching. *)
      (* Can several actions constrain the same agent in incompatible ways? *)

      (* In this example, what is happening? *)
      (* S(a[u/p],b[u,p]), S(c[u/p],d[u,p]) *)

      (* Compute a matching implied by the modifications that may still be incomplete. *)
      (* Is it possible there may be something unsound here? *)
      (* What if {a1->a2} *)
      (* CEX: {S(x[u/p]), S(x[u/p],y[u/p])}: this is going to be rejected *)
      let ag_matching =
        pat.main_pattern.mods
        |> List.fold_left
             (fun acc pat_action ->
               match List.concat_map (match_actions pat_action) actions with
               | [] ->
                   raise No_match
               | PAM eqs :: [] ->
                   eqs
                   |> List.fold_left
                        (fun acc (ag_pid, ag_id) -> IntMap.add ag_pid ag_id acc)
                        acc
               | _ ->
                   Tql_error.(fail Agent_ambiguity) )
             IntMap.empty
      in
      let translate_psite ms (ag_pid, s) =
        try Some (IntMap.find ag_pid ms, s) with Not_found -> None
      in
      let propagate_link_constraints graph links ms =
        links
        |> List.fold_left
             (fun ms (s, s') ->
               match translate_psite ms s with
               | None ->
                   ms
               | Some (src_ag, src_s) -> (
                 match Edges.link_destination src_ag src_s graph with
                 | None ->
                     raise No_match
                 | Some dst ->
                     IntMap.add (psite_ag_id s') (site_ag_id dst) ms ) )
             ms
      in
      let add_neighborhood ms =
        ms
        |> propagate_link_constraints w.previous_state.Replay.graph
             (pattern_tested_links pat)
        |> propagate_link_constraints w.state.Replay.graph
             (pattern_created_links pat)
      in
      let same_cardinal m m' = IntMap.cardinal m = IntMap.cardinal m' in
      let ag_matching = fixpoint same_cardinal add_neighborhood ag_matching in
      let n = Array.length pat.main_pattern.agents in
      (* This fails if the identity of an agent could not be uniquely determined. *)
      let ag_matching =
        Array.init n (fun i ->
            try IntMap.find i ag_matching
            with Not_found -> Tql_error.(fail Agent_ambiguity) )
      in
      Some ag_matching
    with No_match -> None
  else None

(* Once we have a mapping from the agents of [pat] to the agents of [w],
   this function tests that [pat] effectively matches [w] *)

let match_simple_pattern (pat : Query.event_pattern) (w : Streaming.window) :
    int array option =
  match match_agents_in_pattern pat w with
  | Some ag_matchings ->
      let _tests, actions = extract_tests_actions w.step in
      let translate_ag ag_pid = ag_matchings.(ag_pid) in
      let same_agents ag_pid (ag_id, _) = translate_ag ag_pid = ag_id in
      let same_sites (a, s) (a', s') = same_agents a a' && s = s' in
      let site_has_type (kind, s) ((_, kind'), s') = kind = kind' && s = s' in
      let match_action pat_action step_action =
        match (pat_action, step_action) with
        | Create ag_pid, Instantiation.Create (ag, _)
        | Destroy ag_pid, Instantiation.Remove ag ->
            same_agents ag_pid ag
        | Mod_int_state (site, st), Instantiation.Mod_internal (site', st') ->
            same_sites site site' && st = st'
        | Mod_lnk_state (site, Free), Instantiation.Free site' ->
            same_sites site site'
        | Mod_lnk_state (s1, Bound_to s2), Instantiation.Bind (s1', s2')
        | Mod_lnk_state (s1, Bound_to s2), Instantiation.Bind_to (s1', s2') ->
            (same_sites s1 s1' && same_sites s2 s2')
            || (same_sites s1 s2' && same_sites s2 s1')
        | Mod_lnk_state (s, Bound_to_any), Instantiation.Bind (s1, s2)
        | Mod_lnk_state (s, Bound_to_any), Instantiation.Bind_to (s1, s2) ->
            same_sites s s1 || same_sites s s2
        | Mod_lnk_state (s, Bound_to_type t), Instantiation.Bind (s1, s2)
        | Mod_lnk_state (s, Bound_to_type t), Instantiation.Bind_to (s1, s2) ->
            (same_sites s s1 && site_has_type t s2)
            || (same_sites s s2 && site_has_type t s1)
        | _ ->
            false
      in
      let mods_ok =
        pat.main_pattern.mods
        |> List.for_all (fun pat_action ->
               actions |> List.exists (match_action pat_action) )
      in
      let prev_mstate = w.previous_state.Replay.graph in
      let tests_ok =
        pat.main_pattern.tests
        |> List.for_all (fun test ->
               try
                 match test with
                 | Agent_exists ag_pid ->
                     Edges.is_agent
                       (translate_ag ag_pid, p_agent_ty pat ag_pid)
                       prev_mstate
                 | Lnk_state_is ((ag_pid, s), Free) ->
                     Edges.is_free (translate_ag ag_pid) s prev_mstate
                 | Lnk_state_is ((ag_pid, s), Bound_to (ag_pid', s')) ->
                     Edges.link_exists (translate_ag ag_pid) s
                       (translate_ag ag_pid') s' prev_mstate
                 | Lnk_state_is ((ag_pid, s), Bound_to_any) ->
                     not (Edges.is_free (translate_ag ag_pid) s prev_mstate)
                 | Lnk_state_is ((ag_pid, s), Bound_to_type (ag_kind', s')) -> (
                   match
                     Edges.link_destination (translate_ag ag_pid) s prev_mstate
                   with
                   | None ->
                       false
                   | Some ((_, ag_kind''), s'') ->
                       ag_kind' = ag_kind'' && s' = s'' )
                 | Int_state_is ((ag_pid, s), st) ->
                     Edges.get_internal (translate_ag ag_pid) s prev_mstate = st
               with
               (* TODO: this is dirty. We are doing this in case some agent
                      does not exist in the previous state and KaSim throws an exception.
                      KaSim should throw a more specific exception or expose a `valid_agent_id` API. *)
               | Invalid_argument _ ->
                   false
               | e ->
                   Log.warn "TODO: catch more specific exception." ~loc:__LOC__
                     ~details:[Printexc.to_string e] ;
                   false )
      in
      if mods_ok && tests_ok then Some ag_matchings else None
  | None ->
      None

(* How to combine the matchings of both ?
   + The main is used as a reference
   + Otherwise, look at the other
   + If no pattern is provided ? Impossible, we cancel *)

let match_event (ev : Query.event) (w : Streaming.window) : ev_matchings option
    =
  let main_pat_opt = ev.event_pattern in
  let def_pat_opt =
    match ev.defining_rel with
    | Some (First_after (_, pat)) | Some (Last_before (_, pat)) ->
        Some pat
    | _ ->
        None
  in
  let qid_to_gid (pat, matchings) q_id =
    try
      let p_id = IntMap.find q_id pat.main_pattern.agent_constraints in
      Some matchings.(p_id)
    with Not_found -> None
  in
  let qid_to_gid' pm1 pm2_opt q_id =
    match pm2_opt with
    | None -> (
      match qid_to_gid pm1 q_id with None -> assert false | Some gid -> gid )
    | Some pm2 -> (
      match (qid_to_gid pm1 q_id, qid_to_gid pm2 q_id) with
      | None, Some gid | Some gid, None ->
          gid
      | Some gid, Some gid' ->
          assert (gid = gid') ;
          gid
      | None, None ->
          assert false )
  in
  let generate_matchings effective pm1 pm2_opt =
    let common =
      { ev_id_in_trace= w.step_id
      ; ev_id_in_query= ev.event_id
      ; ev_time= w.state.Replay.time
      ; indexing_ag_matchings=
          List.map (qid_to_gid' pm1 pm2_opt) ev.already_constrained_agents }
    in
    let specific =
      { new_ag_matchings= List.map (qid_to_gid' pm1 pm2_opt) ev.captured_agents
      ; recorded_measures= IntMap.empty }
    in
    if effective then {common_to_all= common; matchings= [{common; specific}]}
    else {common_to_all= common; matchings= []}
  in
  match (def_pat_opt, main_pat_opt) with
  | Some def_pat, Some main_pat -> (
    match match_simple_pattern def_pat w with
    | None ->
        None (* No matching at all *)
    | Some def_pat_matchings -> (
      match match_simple_pattern main_pat w with
      | None ->
          Some (generate_matchings false (def_pat, def_pat_matchings) None)
      | Some main_pat_matchings ->
          Some
            (generate_matchings true
               (def_pat, def_pat_matchings)
               (Some (main_pat, main_pat_matchings)) ) ) )
  | Some pat, None | None, Some pat -> (
    match match_simple_pattern pat w with
    | None ->
        None
    | Some pat_matchings ->
        Some (generate_matchings true (pat, pat_matchings) None) )
  | _ ->
      None
