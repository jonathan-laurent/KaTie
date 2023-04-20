(*****************************************************************************)
(* Kappa Trace Query Engine                                                  *)
(*****************************************************************************)

let trace_file = ref ""

let query_file = ref ""

let no_backtraces = ref false

let snapshots_name_format = ref ""

let use_legacy_evaluator = ref false

let export_errors = ref false

let execution_started =
  ref false (* used to differentiate static and dynamic errors *)

let native_snapshots () = Output.snapshots_native_format := true

let usage = Sys.argv.(0) ^ " queries a Kappa trace"

let options =
  [ ("-q", Arg.Set_string query_file, "query file")
  ; ("-t", Arg.Set_string trace_file, "trace file")
  ; ("--legacy", Arg.Set use_legacy_evaluator, "use the legacy evaluator")
  ; ("--no-backtraces", Arg.Set no_backtraces, "disable exception backtraces")
  ; ( "--debug-level"
    , Arg.Set_int Output.debug_level
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
  ; ( "--export-errors"
    , Arg.Set export_errors
    , "export all static errors in an errors.json file" )
  ; ( "--output-dir"
    , Arg.String Output.set_output_directory
    , "set the output directory (default: '.')" ) ]

let parse_queries file =
  try
    let ic = open_in file in
    let lexbuf = Lexing.from_channel ic in
    try
      let asts = Query_parser.queries Query_lexer.token lexbuf in
      close_in ic ; asts
    with Query_parser.Error | Error.User_error {kind= Parse_error; _} ->
      Error.(fail ~loc:(Lexing.lexeme_start_p lexbuf) Parse_error)
  with Sys_error _ -> Error.(fail (File_not_found file))

let compile_and_check model query =
  Log.with_current_query query.Query_ast.query_name (fun () ->
      let query = Query_compile.compile model query in
      Sanity_checks.run query ; query )

let perform_static_checks model queries =
  let errors : Yojson.Safe.t =
    `Assoc
      (List.filter_map
         (fun q ->
           try
             ignore (compile_and_check model q) ;
             None
           with Error.User_error e ->
             Some (q.query_name, [%yojson_of: Error.error_kind] e.kind) )
         queries )
  in
  Output.with_file "errors.json" (fun fmt ->
      Format.fprintf fmt "%a@.]" (Yojson.Safe.pretty_print ~std:false) errors )

let formatter_of_file f = Format.formatter_of_out_channel (open_out f)

let main () =
  Arg.parse options (fun _ -> ()) usage ;
  if not !no_backtraces then Printexc.record_backtrace true ;
  if !snapshots_name_format <> "" then
    Output.set_snapshots_name_format !snapshots_name_format
  else if !Output.snapshots_native_format then
    Output.set_snapshots_name_format "snapshot.%.ka" ;
  if !trace_file = "" || !query_file = "" then Arg.usage options usage
  else
    let header = Trace_header.load ~trace_file:!trace_file in
    let asts = parse_queries !query_file in
    if !export_errors then perform_static_checks header.model asts ;
    Output.debug_json "queries-ast.json" (fun () ->
        [%yojson_of: Query_ast.t list] asts ) ;
    let queries = List.map (compile_and_check header.model) asts in
    Output.debug_json "compiled-queries.json" (fun () ->
        [%yojson_of: Query.t list] queries ) ;
    let queries_and_formatters =
      List.map
        (fun q ->
          (q, formatter_of_file (Output.file ~kind:`Result q.Query.title)) )
        queries
    in
    List.iter
      (fun (_, fmt) -> Format.fprintf fmt "@[<v>")
      queries_and_formatters ;
    let (module Evaluator : Query_evaluator.S) =
      if !use_legacy_evaluator then (module Query_eval_legacy)
      else (module Query_eval)
    in
    execution_started := true ;
    Evaluator.eval_batch ~trace_file:!trace_file queries_and_formatters ;
    List.iter (fun (_, fmt) -> Format.fprintf fmt "@]@.") queries_and_formatters ;
    Terminal.(println [bold; green] "Done.")

let () =
  try main () ; exit 0 with
  (* Normal user errors *)
  | Error.User_error e ->
      Terminal.(println [red] (Error.string_of_error e)) ;
      (* We use return code 1 for a user error that is detected
         statically and 2 for a user error that is detected dynamically. *)
      exit (if !execution_started then 2 else 1)
  | Sys_error msg ->
      (* This typically happens when a file is not found or a directory
         cannot be created. *)
      Terminal.(println [red] ("System Error: " ^ msg)) ;
      exit 1
  (* Internal errors *)
  | (Failure _ | Assert_failure _) as exn ->
      Log.error "A top-level exception was caught." ~exn ;
      exit 3
  | ExceptionDefn.Malformed_Decl _ as exn ->
      Log.error "A KaSim internal exception was raised." ~exn ;
      exit 3
