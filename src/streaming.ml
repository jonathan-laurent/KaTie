(*****************************************************************************)
(* Functions to stream through a trace                                       *)
(*****************************************************************************)

module JS = Yojson.Basic

type window =
  { previous_state: Safe_replay.state
  ; state: Safe_replay.state
  ; step: Trace.step
  ; step_id: int }

(* For each step in the trace, the query engine needs to access the
   state before AND after the underlying transition. Since [Edges.t] is
   imperative, the way we achieve this is by replaying the trace twice
   in parallel, one of them being constantly one step ahead.*)
let fold_trace (type acc) ?(compute_previous_states = true)
    ?(skip_init_events = false) ~(update_ccs : bool) ~(trace_file : string)
    (f : window -> acc -> acc) (init : acc) : acc =
  let init_state () =
    Safe_replay.init_state ~with_connected_components:update_ccs
  in
  let previous = ref None in
  let state = ref (init_state ()) in
  let step_id = ref (-1) in
  let initializing = ref true in
  (* We wrap f in case initial events must be skipped  *)
  let f window acc =
    if not (Trace.step_is_init window.step) then initializing := false ;
    if skip_init_events && !initializing then acc else f window acc
  in
  let process_step model acc raw_step =
    let step =
      Safe_replay.replay_and_translate_step (Model.signatures model) !state
        raw_step
    in
    step_id := !step_id + 1 ;
    match !previous with
    | None ->
        let previous_state = init_state () in
        if compute_previous_states then
          previous := Some (previous_state, raw_step) ;
        let window = {step; previous_state; state= !state; step_id= !step_id} in
        f window acc
    | Some (previous_state, last_raw_step) ->
        let _last_step =
          Safe_replay.replay_and_translate_step (Model.signatures model)
            previous_state last_raw_step
        in
        previous := Some (previous_state, raw_step) ;
        let window = {step; previous_state; state= !state; step_id= !step_id} in
        f window acc
  in
  snd (Trace.fold_trace_file process_step (fun _ -> init) trace_file)
