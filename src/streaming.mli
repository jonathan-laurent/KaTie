type window =
  { previous_state: Replay.state
  ; state: Replay.state
  ; step: Trace.step
  ; step_id: int }

val fold_trace :
     ?update_ccs:bool
  -> ?compute_previous_states:bool
  -> ?skip_init_events:bool
  -> string
  -> (window -> 'a -> 'a)
  -> 'a
  -> 'a
