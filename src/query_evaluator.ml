module type S = sig
  val eval_batch :
    trace_file:string -> (Query.query * Format.formatter) list -> unit
end
