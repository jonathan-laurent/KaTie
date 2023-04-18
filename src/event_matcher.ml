(*****************************************************************************)
(* Event matcher                                                             *)
(*****************************************************************************)

open Query
open Utils
open Streaming

(*****************************************************************************)
(* Simple utilities                                                          *)
(*****************************************************************************)

open Trace_util

let meaningful_step = function
  | Trace.Subs _ | Trace.Dummy _ ->
      false
  | _ ->
      true

let satisfy_rule_constraint_disjunct d step =
  match (d, step) with
  | Init, Trace.Init _ ->
      true
  | Rule r, Trace.Rule (r', _, _) ->
      r = r'
  | _ ->
      false

let satisfy_rule_constraint cs _state step =
  match cs with
  | None ->
      meaningful_step step
  | Some disjs ->
      List.exists (fun d -> satisfy_rule_constraint_disjunct d step) disjs

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

let defining_pattern ev =
  match ev.defining_rel with
  | Some (First_after (_, pat)) | Some (Last_before (_, pat)) ->
      Some pat
  | _ ->
      None

let site_ag_id ((id, _), _) = id

let psite_ag_id (id, _) = id

let agent_ty ag = snd ag

let agent_id ag = fst ag

let p_agent_ty pat ag_pid = pat.main_pattern.agents.(ag_pid).pat_agent_kind

let site_has_type (kind, s) ((_, kind'), s') = kind = kind' && s = s'

let compatible_agents pat ag_pid ag = p_agent_ty pat ag_pid = agent_ty ag

let compatible_sites pat (ag_pid, s) (ag, s') =
  compatible_agents pat ag_pid ag && s = s'

let rec fixpoint eq f x =
  let fx = f x in
  if eq x fx then x else fixpoint eq f fx

(*****************************************************************************)
(* Matching lists and maps                                                   *)
(*****************************************************************************)

type agent_matching_list =
  | AML of (Aliases.pat_agent_id * Aliases.global_agent_id) list

type agent_matching_map =
  | AMM of Aliases.global_agent_id Aliases.pat_agent_id_map

exception No_match

let empty_amm = AMM IntMap.empty

let append (AMM m) (ag_pid, ag_id) =
  AMM
    ( match IntMap.find_opt ag_pid m with
    | Some ag_id' when ag_id' <> ag_id ->
        raise No_match
    | _ ->
        IntMap.add ag_pid ag_id m )

(* We raise [No_match] if we make an incompatible update at any point. *)
let append_aml amm (AML eqs) = List.fold_left (fun m eq -> append m eq) amm eqs

let injective (AMM m) =
  IntMap.bindings m |> List.map snd |> Utils.no_duplicates Int.compare

let check_injective amm = if not (injective amm) then raise No_match

let translate_psite (AMM ms) (ag_pid, s) =
  try Some (IntMap.find ag_pid ms, s) with Not_found -> None

let same_cardinal (AMM m) (AMM m') = IntMap.cardinal m = IntMap.cardinal m'

(*****************************************************************************)
(* Full event matchings                                                      *)
(*****************************************************************************)

(* Maps pattern ids to global ids *)
type full_agent_matching = FAM of Aliases.global_agent_id array

(* Make a full agent matching from an [agent_matching_map]. Raises
   [No_match] if matchings are missing for some local agents. *)
let make_full_agent_matching pat (AMM amm) =
  let n = Array.length pat.main_pattern.agents in
  FAM
    (Array.init n (fun i ->
         try IntMap.find i amm
         with Not_found -> Tql_error.(fail Agent_ambiguity) ) )

let translate_ag (FAM m) ag_pid = m.(ag_pid)

let same_agents fam ag_pid (ag_id, _) = translate_ag fam ag_pid = ag_id

let same_sites fam (a, s) (a', s') = same_agents fam a a' && s = s'

(*****************************************************************************)
(* Discover potential event matching                                         *)
(*****************************************************************************)

(* Take a pattern action and a step action and return all possible
   partial agent matchings that are induced by matching the two
   together. For example, the binding action in the event pattern
   {s1:S(x[./1]), s2:S(x[./1])}, when matched with a concrete binding
   action involving agents a1 and a2 in the trace, induces two matchings
   {s1->a1, s2->a2} and {s1->a2, s2->a1}. *)
let match_action pat pat_action step_action =
  match (pat_action, step_action) with
  | Create ag_pid, Instantiation.Create (ag, _)
  | Destroy ag_pid, Instantiation.Remove ag ->
      if compatible_agents pat ag_pid ag then [AML [(ag_pid, agent_id ag)]]
      else []
  | Mod_int_state (site, st), Instantiation.Mod_internal (site', st') ->
      if compatible_sites pat site site' && st = st' then
        [AML [(psite_ag_id site, site_ag_id site')]]
      else []
  | Mod_lnk_state (site, Free), Instantiation.Free site' ->
      if compatible_sites pat site site' then
        [AML [(psite_ag_id site, site_ag_id site')]]
      else []
  | Mod_lnk_state (s1, Bound_to s2), Instantiation.Bind (s1', s2')
  | Mod_lnk_state (s1, Bound_to s2), Instantiation.Bind_to (s1', s2') ->
      let aux s1 s2 s1' s2' =
        if compatible_sites pat s1 s1' && compatible_sites pat s2 s2' then
          [ AML
              [ (psite_ag_id s1, site_ag_id s1')
              ; (psite_ag_id s2, site_ag_id s2') ] ]
        else []
      in
      aux s1 s2 s1' s2' @ aux s1 s2 s2' s1'
  | Mod_lnk_state (s, Bound_to_any), Instantiation.Bind (s1, s2)
  | Mod_lnk_state (s, Bound_to_any), Instantiation.Bind_to (s1, s2) ->
      let aux s s' =
        if compatible_sites pat s s' then [AML [(psite_ag_id s, site_ag_id s')]]
        else []
      in
      aux s s1 @ aux s s2
  | Mod_lnk_state (s, Bound_to_type t), Instantiation.Bind (s1, s2)
  | Mod_lnk_state (s, Bound_to_type t), Instantiation.Bind_to (s1, s2) ->
      let aux s s1 s2 =
        if compatible_sites pat s s1 && site_has_type t s2 then
          [AML [(psite_ag_id s, site_ag_id s1)]]
        else []
      in
      aux s s1 s2 @ aux s s2 s1
  | _ ->
      []

(* Return the agent matching map induced by the step actions alone. We
   mandate that each pattern action matches unambiguously with a single
   trace action. *)
let add_matchings_implied_by_actions pat step_actions amm =
  pat.main_pattern.mods
  |> List.fold_left
       (fun amm pat_action ->
         match List.concat_map (match_action pat pat_action) step_actions with
         | [] ->
             raise No_match
         | [aml] ->
             append_aml amm aml
         | _ ->
             Tql_error.(fail Agent_ambiguity) )
       amm

(* Take a Kappa mixture and a list of known bindings between local
   agents and augment the map of known agent matchings accordingly. *)
let propagate_link_constraints_with (graph : Edges.t)
    (links : (Aliases.local_site * Aliases.local_site) list) amm =
  links
  |> List.fold_left
       (fun amm (s, s') ->
         match translate_psite amm s with
         | None ->
             amm
         | Some (src_ag, src_s) -> (
           match Edges.link_destination src_ag src_s graph with
           | None ->
               raise No_match
           | Some dst ->
               append amm (psite_ag_id s', site_ag_id dst) ) )
       amm

(* Perform one step of link constraint propagation. *)
let propagate_link_constraints pat w amm =
  let amm =
    amm
    |> propagate_link_constraints_with w.previous_state.Replay.graph
         (pattern_tested_links pat)
    |> propagate_link_constraints_with w.state.Replay.graph
         (pattern_created_links pat)
  in
  check_injective amm ; amm

(* Try to match the agents of [pat] in [w] by only looking at
   modifications and connectivity. Only uses w.step (not the mixture)
   This returns an array that should be interpreted as a map from
   [pat_agent_id] to [global_agent_id]. If [constrs] is provided, the
   algorithm starts with preexisting agent matchings. *)
let match_agents_in_pattern ?(constrs = empty_amm) pat w =
  if satisfy_rule_constraint pat.rule_constraint w.state w.step then
    try
      let _tests, step_actions = extract_tests_actions w.step in
      let amm = add_matchings_implied_by_actions pat step_actions constrs in
      check_injective amm ;
      let amm = fixpoint same_cardinal (propagate_link_constraints pat w) amm in
      (* If we reached this point without [No_match] being thrown, it
         means that every action in the pattern was successfully matched
         and all the link information propagated. Thus, by the local
         rigidity assumption, the identity of all pattern agents must be
         resolved. *)
      Some (make_full_agent_matching pat amm)
    with No_match -> None
  else None

(*****************************************************************************)
(* Check the validity of event matchings                                     *)
(*****************************************************************************)

let test_matching_action fam pat_action step_action =
  match (pat_action, step_action) with
  | Create ag_pid, Instantiation.Create (ag, _)
  | Destroy ag_pid, Instantiation.Remove ag ->
      same_agents fam ag_pid ag
  | Mod_int_state (site, st), Instantiation.Mod_internal (site', st') ->
      same_sites fam site site' && st = st'
  | Mod_lnk_state (site, Free), Instantiation.Free site' ->
      same_sites fam site site'
  | Mod_lnk_state (s1, Bound_to s2), Instantiation.Bind (s1', s2')
  | Mod_lnk_state (s1, Bound_to s2), Instantiation.Bind_to (s1', s2') ->
      (same_sites fam s1 s1' && same_sites fam s2 s2')
      || (same_sites fam s1 s2' && same_sites fam s2 s1')
  | Mod_lnk_state (s, Bound_to_any), Instantiation.Bind (s1, s2)
  | Mod_lnk_state (s, Bound_to_any), Instantiation.Bind_to (s1, s2) ->
      same_sites fam s s1 || same_sites fam s s2
  | Mod_lnk_state (s, Bound_to_type t), Instantiation.Bind (s1, s2)
  | Mod_lnk_state (s, Bound_to_type t), Instantiation.Bind_to (s1, s2) ->
      (same_sites fam s s1 && site_has_type t s2)
      || (same_sites fam s s2 && site_has_type t s1)
  | _ ->
      false

let test_holds pat state fam test =
  try
    match test with
    | Agent_exists ag_pid ->
        Edges.is_agent (translate_ag fam ag_pid, p_agent_ty pat ag_pid) state
    | Lnk_state_is ((ag_pid, s), Free) ->
        Edges.is_free (translate_ag fam ag_pid) s state
    | Lnk_state_is ((ag_pid, s), Bound_to (ag_pid', s')) ->
        Edges.link_exists (translate_ag fam ag_pid) s (translate_ag fam ag_pid')
          s' state
    | Lnk_state_is ((ag_pid, s), Bound_to_any) ->
        not (Edges.is_free (translate_ag fam ag_pid) s state)
    | Lnk_state_is ((ag_pid, s), Bound_to_type (ag_kind', s')) -> (
      match Edges.link_destination (translate_ag fam ag_pid) s state with
      | None ->
          false
      | Some ((_, ag_kind''), s'') ->
          ag_kind' = ag_kind'' && s' = s'' )
    | Int_state_is ((ag_pid, s), st) ->
        Edges.get_internal (translate_ag fam ag_pid) s state = st
  with
  (* TODO: this is dirty. We are doing this in case some agent
         does not exist in the previous state and KaSim throws
         an exception. KaSim should throw a more specific
         exception or expose a `valid_agent_id` API. *)
  | Invalid_argument _ ->
      false
  | e ->
      Log.warn "TODO: catch more specific exception." ~loc:__LOC__
        ~details:[Printexc.to_string e] ;
      false

let check_full_matching pat w fam =
  let _tests, actions = extract_tests_actions w.step in
  let mods_ok =
    pat.main_pattern.mods
    |> List.for_all (fun pat_action ->
           actions |> List.exists (test_matching_action fam pat_action) )
  in
  let tests_ok =
    let state = w.previous_state.Replay.graph in
    pat.main_pattern.tests |> List.for_all (test_holds pat state fam)
  in
  mods_ok && tests_ok

(* Once we have a mapping from the agents of [pat] to the agents of [w],
   this function tests that [pat] effectively matches [w] *)
let match_simple_pattern ?constrs pat w =
  match match_agents_in_pattern ?constrs pat w with
  | Some fam ->
      if check_full_matching pat w fam then Some fam else None
  | None ->
      None

(*****************************************************************************)
(* Main function                                                             *)
(*****************************************************************************)

type status =
  | Failure
  | Success of {other_constrained: Aliases.global_agent_id list}
[@@deriving show, yojson_of]

type potential_matching = {link: Aliases.global_agent_id list; status: status}
[@@deriving show, yojson_of]

(* Returns [None] when the provided local id is not constrained by the
   pattern. *)
let gid_of_lid pat (FAM m) lid =
  try
    let pid = IntMap.find lid pat.main_pattern.agent_constraints in
    Some m.(pid)
  with Not_found -> None

let gid_of_lid_exn pat fam lid = gid_of_lid pat fam lid |> Option.get

(* Given a pattern [pat], a full matching from [pat_id] to [global_id]
   and another pattern [pat'], we want a matching from [pat_id'] to
   [global_id] for [pat']. To do so, we look at the agent constraints of
   [pat']. If such a constraint says lid->pid' and we have lid->gid from
   [pat]'s matching, then we have pid'->gid. The resulting matching
   cannot contain a conflict since all global ids from [pat]'s matching are
   distint. *)
let _transpose_constraints pat fam pat' =
  AMM
    (IntMap.fold
       (fun lid pid' eqs ->
         match gid_of_lid pat fam lid with
         | None ->
             eqs
         | Some gid ->
             IntMap.add pid' gid eqs )
       pat'.agent_constraints IntMap.empty )

let match_event ev w : potential_matching option =
  match (defining_pattern ev, ev.event_pattern) with
  | None, None ->
      assert false
  | Some pat, None | None, Some pat -> (
    match match_simple_pattern pat w with
    | None ->
        None
    | Some fam ->
        let link = List.map (gid_of_lid_exn pat fam) ev.link_agents in
        let status =
          Success
            { other_constrained=
                List.map (gid_of_lid_exn pat fam) ev.other_constrained_agents }
        in
        Some {link; status} )
  | Some def_pat, Some main_pat -> (
    match match_simple_pattern def_pat w with
    | None ->
        None
    | Some _fam -> (
        let constrs = assert false in
        match match_simple_pattern ~constrs main_pat w with _ -> assert false )
    )
