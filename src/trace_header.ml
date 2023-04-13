type t = {uuid: int option; model: Model.t}

let load ~trace_file =
  let uuid, model = Trace.get_headers_from_file trace_file in
  {uuid; model}
