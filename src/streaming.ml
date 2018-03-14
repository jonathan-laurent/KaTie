(*****************************************************************************)
(* Functions to stream through a trace                                       *)
(*****************************************************************************)

module JS = Yojson.Basic

type window = {
    previous_state : Replay.state ;
    state : Replay.state ;
    step : Trace.step ;
    step_id : int ;
}

(* TODO: This is shameful... *)
let extract_env trace_file =
    Trace.fold_trace_file 
        (fun _ _ _ -> ()) 
        (fun _ -> ()) 
        trace_file 
    |> fst


let fold_trace
    (type acc)
    ?(update_ccs=true)
    ?(compute_previous_states=true)
    ?(skip_init_events=false)
    (fname : string)
    (step_f : window -> acc -> acc)
    (init : acc)
    : acc =

    let previous = ref None in
    let init_state () = 
        Replay.init_state ~with_connected_components:update_ccs in
    let state = ref (init_state ()) in
    let step_id = ref (-1) in
    let initializing = ref true in

    let step_f window acc =
        if not (Trace.step_is_init window.step) then
            initializing := false ;
        if skip_init_events && !initializing then acc
        else step_f window acc in

    let process_step model acc step =
        state := Replay.do_step (Model.signatures model) !state step |> fst ;
        step_id := !step_id + 1 ;
        begin match !previous with
        | None ->
            let previous_state = init_state () in
            if compute_previous_states then previous := Some (previous_state, step) ;
            let window = {
                step ;
                previous_state ;
                state = !state ;
                step_id = !step_id ;
            } in step_f window acc 
        | Some (previous_state, last_step) ->
            let previous_state, _ = 
                Replay.do_step (Model.signatures model) previous_state last_step in
            previous := Some (previous_state, step) ;
            let window = {
                step ;
                previous_state ;
                state = !state ;
                step_id = !step_id
            } in step_f window acc 
        end in

    snd (Trace.fold_trace_file process_step (fun _ -> init) fname)