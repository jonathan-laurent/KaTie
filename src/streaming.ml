(*****************************************************************************)
(* Functions to stream through a trace                                       *)
(*****************************************************************************)

module JS = Yojson.Basic

let extract_env (fname : string) = 
    let desc = open_in fname in
  let lex_buf = Lexing.from_channel desc in
  let lex_st = Yojson.init_lexer ~fname () in

  JS.read_space lex_st lex_buf ;
  JS.read_lcurl lex_st lex_buf ;
  JS.read_space lex_st lex_buf ;

  let ident = JS.read_ident lex_st lex_buf in

  assert (ident = "uuid") ;
  JS.read_space lex_st lex_buf ;
  JS.read_colon lex_st lex_buf ;
  JS.read_space lex_st lex_buf ;
  ignore (JS.read_string lex_st lex_buf) ;
  JS.read_space lex_st lex_buf ;
  JS.read_comma lex_st lex_buf ;
  JS.read_space lex_st lex_buf ;

  let ident = JS.read_ident lex_st lex_buf in

  assert (ident = "env") ;
  JS.read_space lex_st lex_buf ;
  JS.read_colon lex_st lex_buf ;
  JS.read_space lex_st lex_buf ;

  let env = Model.of_yojson (JS.read_json lex_st lex_buf) in
  env



let basic_fold_trace (type acc)
    ?(update_ccs=true) 
    (fname : string) 
    (step_f : Trace.step -> Replay.state -> acc -> acc) 
    (init : acc)
    : acc =

  let desc = open_in fname in
  let lex_buf = Lexing.from_channel desc in
  let lex_st = Yojson.init_lexer ~fname () in

  JS.read_space lex_st lex_buf ;
  JS.read_lcurl lex_st lex_buf ;
  JS.read_space lex_st lex_buf ;

  let ident = JS.read_ident lex_st lex_buf in

  assert (ident = "uuid") ;
  JS.read_space lex_st lex_buf ;
  JS.read_colon lex_st lex_buf ;
  JS.read_space lex_st lex_buf ;
  ignore (JS.read_string lex_st lex_buf) ;
  JS.read_space lex_st lex_buf ;
  JS.read_comma lex_st lex_buf ;
  JS.read_space lex_st lex_buf ;

  let ident = JS.read_ident lex_st lex_buf in

  assert (ident = "env") ;
  JS.read_space lex_st lex_buf ;
  JS.read_colon lex_st lex_buf ;
  JS.read_space lex_st lex_buf ;

  let env = Model.of_yojson (JS.read_json lex_st lex_buf) in

  JS.read_space lex_st lex_buf ;
  JS.read_comma lex_st lex_buf ;
  JS.read_space lex_st lex_buf ;

  let ident = JS.read_ident lex_st lex_buf in

  assert (ident = "trace") ;
  JS.read_space lex_st lex_buf ;
  JS.read_colon lex_st lex_buf ;
  JS.read_space lex_st lex_buf ;

  let progress = Progress_report.create 80 '#' in

  try
    let final, acc = JS.read_sequence
        (fun (state, acc) x y ->
           let step = Trace.step_of_yojson (JS.read_json x y) in
           let state, _ = Replay.do_step (Model.signatures env) state step in
            Progress_report.tick 
                state.Replay.time 0. state.Replay.event 0. progress ;
           state, step_f step state acc)
        (Replay.init_state ~with_connected_components:update_ccs, init) lex_st lex_buf in

    Progress_report.complete_progress_bar
        final.Replay.time final.Replay.event progress ;
    
    JS.read_space lex_st lex_buf ;
    (try JS.read_object_end lex_buf with Yojson.End_of_object -> ()) ;
    JS.read_space lex_st lex_buf ;
    close_in desc ;
    acc
  with
  | ExceptionDefn.Internal_Error er
  | ExceptionDefn.Malformed_Decl er ->
    let () = Pp.error Format.pp_print_string er in
    let () = close_in desc in
    exit 2


type window = {
    previous_state : Replay.state ;
    state : Replay.state ;
    step : Trace.step ;
    step_id : int ;
}

let fold_trace
    (type acc)
    ?(update_ccs=true)
    ?(compute_previous_states=true)
    (env : Model.t)
    (fname : string)
    (step_f : window -> acc -> acc)
    (init : acc)
    : acc =

    let previous = ref None in
    let step_id = ref (-1) in

    let init_state = 
        Replay.init_state ~with_connected_components:update_ccs in

    let process_step step state acc =
        step_id := !step_id + 1 ;
        begin match !previous with
        | None ->
            if compute_previous_states then previous := Some (init_state, step) ;
            let window = {
                step ;
                previous_state = init_state ;
                state = state ;
                step_id = !step_id ;
            } in step_f window acc 
        | Some (previous_state, last_step) ->
            let previous_state, _ = 
                Replay.do_step (Model.signatures env) previous_state last_step in
            previous := Some (previous_state, step) ;
            let window = {
                step ;
                previous_state ;
                state = state ;
                step_id = !step_id
            } in step_f window acc 
        end in

    basic_fold_trace ~update_ccs fname process_step init