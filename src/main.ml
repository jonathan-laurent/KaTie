(*****************************************************************************)
(* Kappa Trace Query Engine                                                  *)
(*****************************************************************************)

open Tql_error

let trace_file = ref ""
let query_file = ref ""
let default_out_file = ref "output.csv"
let debug_mode = ref false
let snapshots_name_format = ref ""
let skip_init_events = ref false

let native_snapshots () =
  Tql_output.snapshots_native_format := true

let usage =
  Sys.argv.(0) ^
  " queries a Kappa trace"

let options = [
  "-q", Arg.Set_string query_file, "query file" ;
  "-t", Arg.Set_string trace_file, "trace file" ;
  "-o", Arg.Set_string default_out_file, 
  "default output file (if not specified: `output.csv`)" ;
  "--debug", Arg.Set debug_mode, "set debug mode" ;
  "--skip-init-events", Arg.Set skip_init_events, "skip INIT events in the trace" ;
  "--snapshots-names", Arg.Set_string snapshots_name_format,
  "name format of generated snapshot files (default: 'snapshot.%.json' or 'snapshot.%.ka')" ;
  "--native-snapshots", Arg.Unit native_snapshots,
  "dump snapshot using KaSim's native format" ;
  "--output-directory", Arg.String Tql_output.set_output_directory ,
  "set the output directory (default: '.')" ]

let parse_and_compile_queries model file = 
  try
  let ic = open_in file in
  let lexbuf = Lexing.from_channel ic in
  begin try
    let asts = Query_parser.queries Query_lexer.token lexbuf in
    let queries = List.map (Query_compile.compile model) asts in
    close_in ic ;
    queries
  with
    | Query_parser.Error -> 
      raise (error_at (Lexing.lexeme_start_p lexbuf) Parse_error)
  end
  with Sys_error _ -> raise (error (File_not_found file))

let with_file file f = 
  let oc = open_out file in
  let fmt = Format.formatter_of_out_channel oc in
  f fmt ;
  close_out oc

let formatter_of_file f = 
  Format.formatter_of_out_channel (open_out f)

let query_output_file q =
  let title = Utils.default !default_out_file q.Query.title in
  Format.sprintf "%s" title


let main () =
  
  if !debug_mode then Printexc.record_backtrace true ;

  Arg.parse options (fun _ -> ()) usage ;

  if !snapshots_name_format <> "" then
    Tql_output.set_snapshots_name_format !snapshots_name_format
  else if !Tql_output.snapshots_native_format then
    Tql_output.set_snapshots_name_format "snapshot.%.ka" ;

  if !trace_file = "" || !query_file = "" then
    Arg.usage options usage
  else begin

    let model = Streaming.extract_env !trace_file in

    let queries = parse_and_compile_queries model !query_file in

    print_endline "Queries successfully compiled." ;

    if !debug_mode then begin
      with_file (Tql_output.file "compiled-query") (fun fmt ->
        queries |> List.iter (fun q ->
          Debug_pp.pp_query fmt q
        ))
    end ;
    
    let queries = queries
      |> List.map (fun q -> (q, query_output_file q)) in

    let fmts = queries
        |> List.map snd
        |> List.sort_uniq compare
        |> List.map (fun f -> (f, formatter_of_file (Tql_output.file f))) in
        
    let queries = queries
      |> List.map (fun (q, f) -> (q, List.assoc f fmts) ) in

    fmts |> List.iter (fun (_, fmt) -> Format.fprintf fmt "@[<v>") ;
    Query_eval.eval_queries 
      ~skip_init_events:!skip_init_events
      model queries !trace_file ;
    fmts |> List.iter (fun (_, fmt) -> Format.fprintf fmt "@]@.") ;
    print_endline "Done."
    
  end


let err_formatter = Format.formatter_of_out_channel stderr

let () =
  try main ()
  with
  | Error e -> Tql_error.print_error err_formatter e
  | Failure msg -> prerr_endline ("[Fatal error] " ^ msg)
  | ExceptionDefn.Malformed_Decl msg -> 
    prerr_endline ("[KaSim error] " ^ fst msg)