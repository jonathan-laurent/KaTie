(*****************************************************************************)
(* Event matcher                                                             *)
(*****************************************************************************)

open Ppx_yojson_conv_lib.Yojson_conv
open Query
open Utils
open Streaming

(*****************************************************************************)
(* List monad                                                                *)
(*****************************************************************************)

(* To handle nondeterminism, most computations happen within the list monad. *)

let ( let* ) x f = List.concat_map f x

let return x = [x]

let fail = []

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
  pat.pattern.tests
  |> List.concat_map (function
       | Lnk_state_is (s, Bound_to s') ->
           [(s, s'); (s', s)]
       | _ ->
           [] )

let pattern_created_links pat =
  pat.pattern.mods
  |> List.concat_map (function
       | Mod_lnk_state (s, Bound_to s') ->
           [(s, s'); (s', s)]
       | _ ->
           [] )

let site_ag_id ((id, _), _) = id

let psite_ag_id (id, _) = id

let agent_ty ag = snd ag

let agent_id ag = fst ag

let p_agent_ty pat ag_pid = pat.pattern.agents.(ag_pid).pat_agent_kind

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

let empty_amm = AMM IntMap.empty

let append (AMM m) (ag_pid, ag_id) =
  match IntMap.find_opt ag_pid m with
  | Some ag_id' when ag_id' <> ag_id ->
      fail
  | _ ->
      return (AMM (IntMap.add ag_pid ag_id m))

let append_aml amm (AML eqs) =
  Utils.monadic_fold (fun m eq -> append m eq) amm eqs

let injective (AMM m) =
  IntMap.bindings m |> List.map snd |> Utils.no_duplicates Int.compare

let check_injective amm = if not (injective amm) then fail else return ()

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
  let n = Array.length pat.pattern.agents in
  FAM
    (Array.init n (fun i ->
         try IntMap.find i amm with Not_found -> Error.(fail Agent_ambiguity) )
    )

let translate_ag (FAM m) ag_pid = m.(ag_pid)

let same_agents fam ag_pid (ag_id, _) = translate_ag fam ag_pid = ag_id

let same_sites fam (a, s) (a', s') = same_agents fam a a' && s = s'

(*****************************************************************************)
(* Discover potential event matching                                         *)
(*****************************************************************************)

(* The trace format features two kinds of actions: [Bind] and [Bind_to].
   In spirit, [[Bind (s, s')]] is equivalent to [[Bind_to (s, s'),
   Bind_to (s', s)]]. Only [Bind_to] can be used in initialization steps
   and both are used in standard rules. In order to treat the two
   equivalently in our matching code, we eliminate half of the instances
   of [Bind_to]. *)
let preprocess_bind_to actions =
  List.filter
    (function Instantiation.Bind_to (s, s') -> s < s' | _ -> true)
    actions

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

(* Return the agent matching maps induced by the step actions alone. We
   mandate that each pattern action matches unambiguously with a single
   trace action. *)
let add_matchings_implied_by_actions pat step_actions amm =
  let step_actions = preprocess_bind_to step_actions in
  pat.pattern.mods
  |> Utils.monadic_fold
       (fun amm pat_action ->
         let* step_action = step_actions in
         let* aml = match_action pat pat_action step_action in
         append_aml amm aml )
       amm

(* Take a Kappa mixture and a list of known bindings between local
   agents and augment the map of known agent matchings accordingly. *)
let propagate_link_constraints_with (graph : Safe_replay.Graph.t)
    (links : (Aliases.local_site * Aliases.local_site) list) amm =
  links
  |> Utils.monadic_fold
       (fun amm (s, s') ->
         match translate_psite amm s with
         | None ->
             return amm
         | Some (src_ag, src_s) -> (
           match Safe_replay.Graph.link_destination src_ag src_s graph with
           | None | (exception Safe_replay.Inexisting_agent) ->
               (* Agent [src_ag] may not be in the mixture. See
                  tests/unit/catphos-mini/query.katie *)
               fail
           | Some dst ->
               append amm (psite_ag_id s', site_ag_id dst) ) )
       amm

(* Perform one step of link constraint propagation. *)
let propagate_link_constraints pat w amm =
  let* amm =
    propagate_link_constraints_with
      (Safe_replay.graph w.previous_state)
      (pattern_tested_links pat) amm
  in
  let* amm =
    propagate_link_constraints_with
      (Safe_replay.graph w.state)
      (pattern_created_links pat)
      amm
  in
  let* () = check_injective amm in
  return amm

exception Link_propagation_failure

let propagate_link_constraints_exn pat w amm =
  match propagate_link_constraints pat w amm with
  | [] ->
      raise Link_propagation_failure
  | [amm] ->
      amm
  | _ ->
      assert
        false (* propagating link constraints should not introduce branching *)

let propagate_link_constraints_until_fixpoint pat w amm =
  try return (fixpoint same_cardinal (propagate_link_constraints_exn pat w) amm)
  with Link_propagation_failure -> fail

(* Try to match the agents of [pat] in [w] by only looking at
   modifications and connectivity. Only uses w.step (not the mixture)
   This returns an array that should be interpreted as a map from
   [pat_agent_id] to [global_agent_id]. If [constrs] is provided, the
   algorithm starts with preexisting agent matchings. *)
let match_agents_in_pattern ?(constrs = empty_amm) pat w =
  if satisfy_rule_constraint pat.rule_constraint w.state w.step then
    let _tests, step_actions = extract_tests_actions w.step in
    let* amm = add_matchings_implied_by_actions pat step_actions constrs in
    let* () = check_injective amm in
    let* amm = propagate_link_constraints_until_fixpoint pat w amm in
    (* If we reached this point without failing, it means that every
       action in the pattern was successfully matched and all the link
       information propagated. Thus, by the local rigidity assumption,
       the identity of all pattern agents must be resolved. *)
    return (make_full_agent_matching pat amm)
  else fail

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
  let module G = Safe_replay.Graph in
  try
    match test with
    | Agent_exists ag_pid ->
        G.is_agent (translate_ag fam ag_pid, p_agent_ty pat ag_pid) state
    | Lnk_state_is ((ag_pid, s), Free) ->
        G.is_free (translate_ag fam ag_pid) s state
    | Lnk_state_is ((ag_pid, s), Bound_to (ag_pid', s')) ->
        G.link_exists (translate_ag fam ag_pid) s (translate_ag fam ag_pid') s'
          state
    | Lnk_state_is ((ag_pid, s), Bound_to_any) ->
        not (G.is_free (translate_ag fam ag_pid) s state)
    | Lnk_state_is ((ag_pid, s), Bound_to_type (ag_kind', s')) -> (
      match G.link_destination (translate_ag fam ag_pid) s state with
      | None ->
          false
      | Some ((_, ag_kind''), s'') ->
          ag_kind' = ag_kind'' && s' = s'' )
    | Int_state_is ((ag_pid, s), st) ->
        G.get_internal (translate_ag fam ag_pid) s state = st
  with Safe_replay.Inexisting_agent -> false

let check_full_matching pat w fam =
  let _tests, actions = extract_tests_actions w.step in
  let mods_ok =
    pat.pattern.mods
    |> List.for_all (fun pat_action ->
           actions |> List.exists (test_matching_action fam pat_action) )
  in
  let tests_ok =
    let state = Safe_replay.graph w.previous_state in
    pat.pattern.tests |> List.for_all (test_holds pat state fam)
  in
  mods_ok && tests_ok

(* Once we have a mapping from the agents of [pat] to the agents of [w],
   this function tests that [pat] effectively matches [w]. *)
let match_simple_pattern ?constrs pat w =
  let* fam = match_agents_in_pattern ?constrs pat w in
  if check_full_matching pat w fam then return fam else fail

(* TODO: do we have to deduplicate matchings after a call to
   [match_simple_pattern]? I do not believe so since branching should
   never yield to identical matchings. *)

(*****************************************************************************)
(* Main function                                                             *)
(*****************************************************************************)

(* A list of possible matchings that all share the same mapping for the
   link agents *)
type related_matchings =
  { link: Aliases.global_agent_id list
  ; other_constrained: Aliases.global_agent_id list list }
[@@deriving show, yojson_of]

(* Returns [None] when the provided local id is not constrained by the
   pattern. *)
let gid_of_lid pat (FAM m) lid =
  try
    let pid = IntMap.find lid pat.pattern.agent_constraints in
    Some m.(pid)
  with Not_found -> None

let gid_of_lid_exn pat fam lid = gid_of_lid pat fam lid |> Option.get

let gid_of_lid_2 pat fam pat' fam' lid =
  match gid_of_lid pat fam lid with
  | Some gid ->
      Some gid
  | None ->
      gid_of_lid pat' fam' lid

let gid_of_lid_2_exn pat fam pat' fam' lid =
  gid_of_lid_2 pat fam pat' fam' lid |> Option.get

(* Group matchings that share a link valuation together *)
let group_matchings ev pat fams =
  fams
  |> List.map (fun fam ->
         let link = List.map (gid_of_lid_exn pat fam) ev.link_agents in
         (link, fam) )
  |> Utils.sort_and_group_list ~compare:(List.compare Int.compare)

(* Given a pattern [pat], a full matching from [pat_id] to [global_id]
   and another pattern [pat'], we want a matching from [pat_id'] to
   [global_id] for [pat']. To do so, we look at the agent constraints of
   [pat']. If such a constraint says lid->pid' and we have lid->gid from
   [pat]'s matching, then we have pid'->gid. The resulting matching
   cannot contain a conflict since all global ids from [pat]'s matching are
   distint. *)
let transpose_constraints pat fam pat' =
  AMM
    (IntMap.fold
       (fun lid pid' eqs ->
         match gid_of_lid pat fam lid with
         | None ->
             eqs
         | Some gid ->
             IntMap.add pid' gid eqs )
       pat'.pattern.agent_constraints IntMap.empty )

let match_event ev w : related_matchings list =
  match (Query.defining_pattern ev, ev.event_pattern) with
  | None, None ->
      assert false
  | Some pat, None | None, Some pat ->
      let fams = match_simple_pattern pat w in
      let* link, fams = group_matchings ev pat fams in
      let other_constrained =
        List.map
          (fun fam ->
            List.map (gid_of_lid_exn pat fam) ev.other_constrained_agents )
          fams
      in
      return {link; other_constrained}
  | Some def_pat, Some main_pat ->
      let fams = match_simple_pattern def_pat w in
      let* link, fams = group_matchings ev def_pat fams in
      let other_constrained =
        let* fam = fams in
        let constrs = transpose_constraints def_pat fam main_pat in
        let* fam' = match_simple_pattern ~constrs main_pat w in
        return
          (List.map
             (gid_of_lid_2_exn def_pat fam main_pat fam')
             ev.other_constrained_agents )
      in
      return {link; other_constrained}
