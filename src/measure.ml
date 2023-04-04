(*****************************************************************************)
(* State and Event Measures                                                  *)
(*****************************************************************************)

let f () = print_endline "Hello !"

open Aliases

type state_measure =
  | Int_state of agent_id * site_id
  | Component of agent_id
  | Print_cc of site_id
  | Snapshot

type event_measure = Time | Rule | Init_event

type state_measure_time = Before | After

type measure =
  | State_measure of state_measure_time * state_measure
  | Event_measure of event_measure

(* Measure implementations *)

let rule_name model event =
  let name =
    match event with
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
  in
  Some (Value.String name)

let component ag_id state =
  match state.Replay.connected_components with
  | None ->
      Log.warn "No connected component information available." ;
      None
  | Some ccs -> (
      let cc_id = Edges.get_connected_component ag_id state.Replay.graph in
      match
        Option.bind cc_id (fun cc_id -> Mods.IntMap.find_option cc_id ccs)
      with
      | None ->
          Log.(
            failwith
              (fmt "Impossible to find the connected component of agent %d."
                 ag_id )
              ~details:
                [ fmt "`cc_id` is equal to None: %b." (cc_id = None)
                ; fmt "`ag_id` is a valid ID: %b."
                    (Edges.is_agent_id ag_id state.Replay.graph) ] )
      | Some cc ->
          Some (Value.Agent_set cc) )

let time state = Some (Value.Float state.Replay.time)

let int_state model (ag_id, ag_site) state =
  let graph = state.Replay.graph in
  let ag_kind = Edges.get_sort ag_id graph in
  let st =
    try Edges.get_internal ag_id ag_site graph
    with e ->
      Log.warn "TODO: catch more specific exception." ~loc:__LOC__
        ~details:[Printexc.to_string e] ;
      Log.(
        error "Unable to access agent's internal state"
          ~details:
            [ fmt "(%d, %d), %d" ag_id ag_kind ag_site
            ; fmt "%d" state.Replay.event
            ; pp Edges.debug_print graph ] ) ;
      assert false
  in
  let signature = Model.signatures model in
  Some
    (Value.String
       (Fmt.to_to_string
          (Signature.print_internal_state signature ag_kind ag_site)
          st ) )

let take_snapshot ?uuid model state file =
  let graph = state.Replay.graph in
  let signature = Model.signatures model in
  let snapshot = Edges.build_snapshot ~raw:true signature graph in
  let snapshot =
    { Data.snapshot_event= state.Replay.event
    ; Data.snapshot_time= state.Replay.time
    ; Data.snapshot_agents=
        Snapshot.export ~raw:true ~debugMode:true signature snapshot
    ; Data.snapshot_tokens= [||] }
  in
  let oc = open_out file in
  ( if !Tql_output.snapshots_native_format then
      let fmt = Format.formatter_of_out_channel oc in
      Data.print_snapshot ?uuid fmt snapshot
    else
      (* guesstimate buffer size: 10x the number of agents *)
      let a_num = List.length snapshot.snapshot_agents in
      let outbuf = Buffer.create (10 * a_num) in
      Data.write_snapshot outbuf snapshot ;
      Buffer.output_buffer oc outbuf ) ;
  close_out oc

let print_cc model state ag_id =
  let edges = state.Replay.graph in
  let ugraph =
    Edges.species ~debugMode:false (Model.signatures model) ag_id edges
  in
  Some (Value.String (Fmt.str "@[<h>%a@]" User_graph.print_cc ugraph))

(* Measure interpreter *)

let take_measure ?(uuid : int option) (model : Model.t)
    (ag_matchings : int array) (w : Streaming.window) measure : Value.t option =
  match measure with
  | State_measure (time, measure) -> (
      let state =
        match time with Before -> w.previous_state | After -> w.state
      in
      match measure with
      | Component ag_id ->
          component ag_matchings.(ag_id) state (* Absurd *)
      | Int_state (ag_id, ag_site) ->
          int_state model (ag_matchings.(ag_id), ag_site) state
      | Snapshot ->
          let filename = Tql_output.new_snapshot_file () in
          take_snapshot ?uuid model state filename ;
          Some (Value.String filename)
      | Print_cc ag_id ->
          print_cc model state ag_matchings.(ag_id) )
  | Event_measure measure -> (
    match measure with
    | Time ->
        time w.state
    | Rule ->
        rule_name model w.step
    | Init_event ->
        Some (Value.Bool (Trace.step_is_init w.step)) )
