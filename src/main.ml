(*****************************************************************************)
(* Kappa Trace Query Engine                                                  *)
(*****************************************************************************)

let trace_file = ref ""
let query_file = ref ""

let usage =
  Sys.argv.(0) ^
  " queries a Kappa trace"

let options = [
  "-q", Arg.Set_string query_file, "query file" ;
  "-t", Arg.Set_string trace_file, "trace file" ]

let parse_and_compile_queries model file = 
  try
    let oc = open_in file in
    let lexbuf = Lexing.from_channel oc in
    let asts = Query_parser.queries Query_lexer.token lexbuf in
    let queries = List.map (Query_compile.compile model) asts in
    close_in oc ;
    queries

  with Sys_error _ -> failwith "File not found."


let with_file file f = 
  let oc = open_out file in
  let fmt = Format.formatter_of_out_channel oc in
  f fmt ;
  close_out oc

let formatter_of_file f = 
  Format.formatter_of_out_channel (open_out f)

let query_output_file q =
  let title = Utils.default "output" q.Query.title in
  Format.sprintf "%s.log" title



let main () =
  let () = Printexc.record_backtrace true in
  let () =
    Arg.parse
      options
      (fun _ -> ())
      usage in
  if !trace_file = "" || !query_file = "" then
    Arg.usage options usage
  else
    let model = Streaming.extract_env !trace_file in

    let queries = parse_and_compile_queries model !query_file in

    print_endline "Queries successfully compiled." ;

    with_file "compiled-query" (fun fmt ->
      queries |> List.iter (fun q ->
        Debug_pp.pp_query fmt q
      )
    );
    
    let queries = queries
      |> List.map (fun q -> (q, query_output_file q)) in

    let fmts = queries
        |> List.map snd
        |> List.sort_uniq compare
        |> List.map (fun f -> (f, formatter_of_file f)) in
        
    let queries = queries
      |> List.map (fun (q, f) -> (q, List.assoc f fmts) ) in

    fmts |> List.iter (fun (_, fmt) -> Format.fprintf fmt "@[<v>") ;
    Query_eval.eval_queries model queries !trace_file ;
    fmts |> List.iter (fun (_, fmt) -> Format.fprintf fmt "@]") ;
    print_endline "Done."


let () = main ()