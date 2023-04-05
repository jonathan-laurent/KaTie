(*****************************************************************************)
(* Measures                                                                  *)
(*****************************************************************************)

open Query
open Streaming

let rule_name model =
  let ret s = Some (Val (s, String)) in
  function
  | Trace.Rule (r, _, _) ->
      let name =
        Format.asprintf "%a" (Model.print_rule ~noCounters:false ~env:model) r
      in
      ret name
  | Trace.Init _ ->
      ret "_init_"
  | Trace.Dummy _ ->
      ret "_dummy_"
  | Trace.Subs _ ->
      ret "_subs_"
  | Trace.Pert _ ->
      ret "_pert_"
  | Trace.Obs _ ->
      ret "_obs_"

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
            error
              (fmt "Impossible to find the connected component of agent %d."
                 ag_id )
              ~details:
                [ fmt "`cc_id` is equal to None: %b." (cc_id = None)
                ; fmt "`ag_id` is a valid ID: %b."
                    (Edges.is_agent_id ag_id state.Replay.graph) ] ;
            assert false )
      | Some cc ->
          (* assert (Agent.SetMap.Set.exists (fun (ag_id', _) -> ag_id = ag_id') cc) ; *)
          Some (Val (cc, Agent_set)) )

let time state = Some (Val (state.Replay.time, Float))

let[@warning "-21"] int_state model ((ag_id, ag_kind), ag_site) state =
  let st =
    try Edges.get_internal ag_id ag_site state.Replay.graph
    with e ->
      Log.warn "TODO: catch more specific exception." ~loc:__LOC__
        ~details:[Printexc.to_string e] ;
      Log.(
        error "Unable to access agent's internal state"
          ~details:
            [ fmt "(%d, %d), %d" ag_id ag_kind ag_site
            ; fmt "%d" state.Replay.event
            ; pp Edges.debug_print state.Replay.graph ] ) ;
      assert false
  in
  let signature = Model.signatures model in
  Some
    (Val
       ( Format.asprintf "%a"
           (Signature.print_internal_state signature ag_kind ag_site)
           st
       , String ) )

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
  let str = Format.asprintf "@[<h>%a@]" User_graph.print_cc ugraph in
  Some (Val (str, String))

let take_measure ?(uuid : int option) (model : Model.t)
    (ag_matchings : int array) (w : Streaming.window) {measure_descr; _} :
    value option =
  match measure_descr with
  | State_measure (time, _ty, st_measure) -> (
      let state =
        match time with Before -> w.previous_state | After -> w.state
      in
      match st_measure with
      | Count _ ->
          None
      | Component ag_id ->
          component ag_matchings.(ag_id) state (* Absurd *)
      | Nphos _ ->
          None
      | Int_state ((ag_id, ag_kind), ag_site) ->
          int_state model ((ag_matchings.(ag_id), ag_kind), ag_site) state
      | Snapshot ->
          let filename = Tql_output.new_snapshot_file () in
          take_snapshot ?uuid model state filename ;
          Some (Val (filename, String))
      | Print_cc ag_id ->
          print_cc model state ag_matchings.(ag_id) )
  | Event_measure (_ty, ev_measure) -> (
    match ev_measure with
    | Time ->
        time w.state
    | Rule ->
        rule_name model w.step
    | Init_event ->
        Some (Val (Trace.step_is_init w.step, Bool))
    | Debug_event ->
        Some
          (Val
             ( Fmt.str "@[<h>%a@]"
                 (Trace.print_step ~compact:false ~env:model)
                 w.step
             , String ) ) )
