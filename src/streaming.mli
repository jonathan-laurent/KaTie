type window =
  { previous_state: Safe_replay.state
  ; state: Safe_replay.state
  ; step: Trace.step
  ; step_id: int }

val fold_trace :
     ?compute_previous_states:bool
  -> ?skip_init_events:bool
  -> update_ccs:bool
  -> trace_file:string
  -> (window -> 'a -> 'a)
  -> 'a
  -> 'a
