(*****************************************************************************)
(* Kappa Trace Query Engine                                                  *)
(*****************************************************************************)

let trace_file = ref ""

let query_file = ref ""

let no_backtraces = ref false

let snapshots_name_format = ref ""

let use_legacy_evaluator = ref false

let native_snapshots () = Tql_output.snapshots_native_format := true

let usage = Sys.argv.(0) ^ " queries a Kappa trace"

let options =
  [ ("-q", Arg.Set_string query_file, "query file")
  ; ("-t", Arg.Set_string trace_file, "trace file")
  ; ("--legacy", Arg.Set use_legacy_evaluator, "use the legacy evaluator")
  ; ("--no-backtraces", Arg.Set no_backtraces, "disable exception backtraces")
  ; ( "--debug-level"
    , Arg.Set_int Tql_output.debug_level
    , "set the debug level (0, 1, 2) (default: 1)" )
  ; ( "--snapshots-names"
    , Arg.Set_string snapshots_name_format
    , "name format of generated snapshot files (default: 'snapshot.%.json' or \
       'snapshot.%.ka')" )
  ; ( "--native-snapshots"
    , Arg.Unit native_snapshots
    , "dump snapshot using KaSim's native format" )
  ; ("--no-color", Arg.Set Terminal.no_color, "disable colored output")
  ; ( "--no-progress-bars"
    , Arg.Set Terminal.disable_progress_bars
    , "disable progress bars to avoid polluting stdout" )
  ; ( "--output-dir"
    , Arg.String Tql_output.set_output_directory
    , "set the output directory (default: '.')" ) ]

let parse_queries file =
  try
    let ic = open_in file in
    let lexbuf = Lexing.from_channel ic in
    try
      let asts = Query_parser.queries Query_lexer.token lexbuf in
      close_in ic ; asts
    with Query_parser.Error | Tql_error.User_error {kind= Parse_error; _} ->
      Tql_error.(fail ~loc:(Lexing.lexeme_start_p lexbuf) Parse_error)
  with Sys_error _ -> Tql_error.(fail (File_not_found file))

let formatter_of_file f = Format.formatter_of_out_channel (open_out f)

let main () =
  Arg.parse options (fun _ -> ()) usage ;
  if not !no_backtraces then Printexc.record_backtrace true ;
  if !snapshots_name_format <> "" then
    Tql_output.set_snapshots_name_format !snapshots_name_format
  else if !Tql_output.snapshots_native_format then
    Tql_output.set_snapshots_name_format "snapshot.%.ka" ;
  if !trace_file = "" || !query_file = "" then Arg.usage options usage
  else
    let header = Trace_header.load ~trace_file:!trace_file in
    let asts = parse_queries !query_file in
    Tql_output.debug_json "queries-ast.json" (fun () ->
        [%yojson_of: Query_ast.t list] asts ) ;
    let queries = List.map (Query_compile.compile header.model) asts in
    Tql_output.debug_json "compiled-queries.json" (fun () ->
        [%yojson_of: Query.t list] queries ) ;
    let queries_and_formatters =
      List.map
        (fun q ->
          (q, formatter_of_file (Tql_output.file ~kind:`Result q.Query.title))
          )
        queries
    in
    List.iter
      (fun (_, fmt) -> Format.fprintf fmt "@[<v>")
      queries_and_formatters ;
    let (module Evaluator : Query_evaluator.S) =
      if !use_legacy_evaluator then (module Query_eval_legacy)
      else (module Query_eval)
    in
    Evaluator.eval_batch ~trace_file:!trace_file queries_and_formatters ;
    List.iter (fun (_, fmt) -> Format.fprintf fmt "@]@.") queries_and_formatters ;
    Terminal.(println [bold; green] "Done.")

let () =
  try main () ; exit 0 with
  (* Normal user errors *)
  | Tql_error.User_error e ->
      Terminal.(println [red] (Tql_error.string_of_error e)) ;
      exit 1
  | Sys_error msg ->
      (* This typically happens when a file is not found or a directory
         cannot be created. *)
      Terminal.(println [red] ("System Error: " ^ msg)) ;
      exit 1
  (* Internal errors *)
  | (Failure _ | Assert_failure _) as exn ->
      Log.error "A top-level exception was caught." ~exn ;
      exit 2
  | ExceptionDefn.Malformed_Decl _ as exn ->
      Log.error "A KaSim internal exception was raised." ~exn ;
      exit 2
