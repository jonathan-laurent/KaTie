(*****************************************************************************)
(* Utilities for manipulating and inspecting trace files                     *)
(*****************************************************************************)

let init_label = "_init_"

let actions_from_side_effects ev =
  let open Instantiation in
  List.map (fun (s, _) -> Free s) ev.side_effects_src
  @ List.map (fun s -> Free s) ev.side_effects_dst

let extract_tests_actions = function
  | Trace.Subs _ ->
      ([], [])
  | Trace.Rule (_, ev, _) ->
      ( ev.Instantiation.tests
      , ev.Instantiation.actions @ actions_from_side_effects ev )
  | Trace.Pert (_, ev, _) ->
      (ev.Instantiation.tests, ev.Instantiation.actions)
  | Trace.Init acts ->
      ([], acts)
  | Trace.Obs (_, tests, _) ->
      (tests, [])
  | Trace.Dummy _ ->
      ([], [])

let rule_name model step =
  match step with
  | Trace.Rule (r, _, _) ->
      let name =
        Fmt.to_to_string (Model.print_rule ~noCounters:false ~env:model) r
      in
      name
  | Trace.Init _ ->
      init_label
  | Trace.Dummy _ ->
      "_dummy_"
  | Trace.Subs _ ->
      "_subs_"
  | Trace.Pert _ ->
      "_pert_"
  | Trace.Obs _ ->
      "_obs_"

let show_internal model sort site st =
  let signature = Model.signatures model in
  Fmt.to_to_string (Signature.print_internal_state signature sort site) st

let show_quark_internal model (ag, site) st =
  show_internal model (Agent.sort ag) site st

let show_site model kind site =
  let signature = Model.signatures model in
  Fmt.to_to_string (Signature.print_site signature kind) site

let show_agent_kind model ag_kind =
  let signature = Model.signatures model in
  Fmt.to_to_string (Signature.print_agent signature) ag_kind

let show_agent model ag =
  let ag_id, ag_kind = (Agent.id ag, Agent.sort ag) in
  let kind = show_agent_kind model ag_kind in
  Fmt.str "%s.%d" kind ag_id

let show_quark model (ag, site) =
  show_agent model ag ^ "." ^ show_site model (Agent.sort ag) site

let dump_action model =
  let open Instantiation in
  function
  | Create (ag, _) ->
      Fmt.str "new(%s)" (show_agent model ag)
  | Remove ag ->
      Fmt.str "del(%s)" (show_agent model ag)
  | Mod_internal (q, st) ->
      Fmt.str "mod(%s, %s)" (show_quark model q)
        (show_quark_internal model q st)
  | Free q ->
      Fmt.str "free(%s)" (show_quark model q)
  | Bind (q, q') ->
      Fmt.str "bind(%s, %s)" (show_quark model q) (show_quark model q')
  | Bind_to (q, q') ->
      Fmt.str "bind-to(%s, %s)" (show_quark model q) (show_quark model q')

(* Trace tests are not read by the engine but we print those anyway. *)
let dump_test model =
  let open Instantiation in
  function
  | Is_Here ag ->
      Fmt.str "ex(%s)" (show_agent model ag)
  | Has_Internal (q, st) ->
      Fmt.str "int(%s, %s)" (show_quark model q)
        (show_quark_internal model q st)
  | Is_Free q ->
      Fmt.str "free(%s)" (show_quark model q)
  | Is_Bound q ->
      Fmt.str "bound(%s)" (show_quark model q)
  | Has_Binding_type (q, (ag_kind, site)) ->
      Fmt.str "bound-to(%s, %s.%s)" (show_quark model q)
        (show_agent_kind model ag_kind)
        (show_site model ag_kind site)
  | Is_Bound_to (q, q') ->
      Fmt.str "bound-to(%s, %s)" (show_quark model q) (show_quark model q')

let dump_binding_state model =
  let open Instantiation in
  function
  | ANY ->
      "#"
  | FREE ->
      "."
  | BOUND ->
      "_"
  | BOUND_TYPE (ag_kind, site) ->
      Fmt.str "%s.%s"
        (show_site model ag_kind site)
        (show_agent_kind model ag_kind)
  | BOUND_to q ->
      show_quark model q

let dump_side_effect_src model (q, bst) =
  Fmt.str "%s{%s}" (show_quark model q) (dump_binding_state model bst)

let dump_side_effect_dst = show_quark

let dump_actions model actions =
  String.concat " " (List.map (dump_action model) actions)

let dump_test_group model tests =
  String.concat " " (List.map (dump_test model) tests)

let dump_tests model tests =
  String.concat " | " (List.map (dump_test_group model) tests)

let if_not_empty label elts =
  match elts with [] -> [] | _ -> [(label, `String (String.concat " " elts))]

let dump_event model e : Yojson.Safe.t =
  let open Instantiation in
  `Assoc
    ( [("actions", `String (dump_actions model e.actions))]
    @ if_not_empty "side-effects-dst"
        (List.map (dump_side_effect_dst model) e.side_effects_dst)
    @ if_not_empty "side-effects-src"
        (List.map (dump_side_effect_src model) e.side_effects_src)
    @ [("tests", `String (dump_tests model e.tests))] )

(* Dump all step actions, including the ones derived from side effects *)
let dump_step_actions model step =
  let _, actions = extract_tests_actions step in
  dump_actions model actions

(* Dump a trace step in a human-readable fashion *)
let dump_step model step =
  match step with
  | Trace.Subs (i, j) ->
      `String (Fmt.str "subs: %d -> %d" i j)
  | Rule (_, e, _) ->
      `List [`String (rule_name model step); dump_event model e]
  | Pert (_, e, _) ->
      `List [`String (rule_name model step); dump_event model e]
  | Init a ->
      `List [`String (rule_name model step); `String (dump_actions model a)]
  | Obs (s, t, _) ->
      `List [`String (Fmt.str "obs: %s" s); `String (dump_tests model t)]
  | Dummy s ->
      `String (Fmt.str "dummy: %s" s)
