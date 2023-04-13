module type S = sig
  val eval_batch :
    trace_file:string -> (Query.t * Format.formatter) list -> unit
end
