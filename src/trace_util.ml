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
      "_init_"
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

let show_site model kind site =
  let signature = Model.signatures model in
  Fmt.to_to_string (Signature.print_site signature kind) site

let show_agent model ag =
  let ag_id, ag_kind = (Agent.id ag, Agent.sort ag) in
  let signature = Model.signatures model in
  let kind = Fmt.to_to_string (Signature.print_agent signature) ag_kind in
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
  | Mod_internal (((a, s) as q), st) ->
      Fmt.str "mod(%s, %s)" (show_quark model q)
        (show_internal model (Agent.sort a) s st)
  | Free q ->
      Fmt.str "free(%s)" (show_quark model q)
  | Bind (q, q') | Bind_to (q, q') ->
      Fmt.str "bind(%s, %s)" (show_quark model q) (show_quark model q')

let dump_step_actions model step =
  let _, actions = extract_tests_actions step in
  String.concat " " (List.map (dump_action model) actions)
