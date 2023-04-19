(*****************************************************************************)
(* State and Event Measures                                                  *)
(*****************************************************************************)

open Aliases
open Value

type state_measure =
  | Int_state of (local_agent_id * site_id)
  | Component of local_agent_id
  | Print_cc of site_id
  | Snapshot
[@@deriving show, yojson_of]

type event_measure = Time | Rule | Debug_event | Init_event
[@@deriving show, yojson_of]

type state_measure_time = Before | After [@@deriving show, yojson_of]

type t =
  | State_measure of state_measure_time * state_measure
  | Event_measure of event_measure
[@@deriving show, yojson_of]

(* Measure implementations *)

let int_state model (ag_id, ag_site) state =
  let graph = Safe_replay.graph state in
  let ag_kind = Safe_replay.Graph.get_sort ag_id graph in
  let st = Safe_replay.Graph.get_internal ag_id ag_site graph in
  VString (Trace_util.show_internal model ag_kind ag_site st)

let take_snapshot ?uuid model state file =
  let graph = Safe_replay.graph state in
  let signature = Model.signatures model in
  let snapshot = Safe_replay.Graph.build_snapshot ~raw:true signature graph in
  let snapshot =
    { Data.snapshot_event= Safe_replay.event state
    ; Data.snapshot_time= Safe_replay.time state
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
  let ugraph =
    Safe_replay.Graph.species ~debugMode:false (Model.signatures model) ag_id
      (Safe_replay.graph state)
  in
  VString (Fmt.str "@[<h>%a@]" User_graph.print_cc ugraph)

(* Measure interpreter *)

let take_measure ~header ag_matching w measure =
  let {Trace_header.uuid; model} = header in
  match measure with
  | State_measure (time, measure) -> (
      let state =
        match time with
        | Before ->
            w.Streaming.previous_state
        | After ->
            w.state
      in
      match measure with
      | Component ag_id ->
          VAgentSet (Safe_replay.connected_component (ag_matching ag_id) state)
      | Int_state (ag_id, ag_site) ->
          int_state model (ag_matching ag_id, ag_site) state
      | Snapshot ->
          let filename = Tql_output.new_snapshot_file () in
          take_snapshot ?uuid model state filename ;
          VString filename
      | Print_cc ag_id ->
          print_cc model state (ag_matching ag_id) )
  | Event_measure measure -> (
    match measure with
    | Time ->
        VFloat (Safe_replay.time w.state)
    | Rule ->
        VString (Trace_util.rule_name model w.step)
    | Init_event ->
        VBool (Trace.step_is_init w.step)
    | Debug_event ->
        VString (Trace_util.dump_step_actions model w.step) )
