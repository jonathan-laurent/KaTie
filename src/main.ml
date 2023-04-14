(*****************************************************************************)
(* Kappa Trace Query Engine                                                  *)
(*****************************************************************************)

let trace_file = ref ""

let query_file = ref ""

let default_out_file = ref "output.csv"

let no_backtraces = ref false

let snapshots_name_format = ref ""

let use_legacy_evaluator = ref false

let no_color = ref false

let native_snapshots () = Tql_output.snapshots_native_format := true

let usage = Sys.argv.(0) ^ " queries a Kappa trace"

let options =
  [ ("-q", Arg.Set_string query_file, "query file")
  ; ("-t", Arg.Set_string trace_file, "trace file")
  ; ( "-o"
    , Arg.Set_string default_out_file
    , "default output file (if not specified: `output.csv`)" )
  ; ("--legacy", Arg.Set use_legacy_evaluator, "use the legacy evaluator")
  ; ("--no-backtraces", Arg.Set no_backtraces, "disable exception backtraces")
  ; ("--debug", Arg.Set Tql_output.debug_mode, "enable debug mode")
  ; ( "--snapshots-names"
    , Arg.Set_string snapshots_name_format
    , "name format of generated snapshot files (default: 'snapshot.%.json' or \
       'snapshot.%.ka')" )
  ; ( "--native-snapshots"
    , Arg.Unit native_snapshots
    , "dump snapshot using KaSim's native format" )
  ; ("--no-color", Arg.Set no_color, "disable colored output")
  ; ( "--output-directory"
    , Arg.String Tql_output.set_output_directory
    , "set the output directory (default: '.')" ) ]

let print_endline_styled s msg =
  if !no_color then print_endline msg
  else ANSITerminal.(print_string s (msg ^ "\n"))

let red, green = ANSITerminal.(red, green)

let parse_and_compile_queries model file =
  try
    let ic = open_in file in
    let lexbuf = Lexing.from_channel ic in
    try
      let asts = Query_parser.queries Query_lexer.token lexbuf in
      let queries = List.map (Query_compile.compile model) asts in
      close_in ic ; (queries, asts)
    with Query_parser.Error ->
      Tql_error.(fail ~loc:(Lexing.lexeme_start_p lexbuf) Parse_error)
  with Sys_error _ -> Tql_error.(fail (File_not_found file))

let formatter_of_file f = Format.formatter_of_out_channel (open_out f)

let query_output_file q =
  let title = Option.value ~default:!default_out_file q.Query.title in
  Format.sprintf "%s" title

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
    let queries, asts = parse_and_compile_queries header.model !query_file in
    print_endline "Queries successfully compiled." ;
    Tql_output.debug_json "queries-ast.json" [%yojson_of: Query_ast.t list] asts ;
    Tql_output.debug_json "compiled-queries.json" [%yojson_of: Query.t list]
      queries ;
    Tql_output.debug_json "execution-paths.json"
      [%yojson_of: (string option * string) list]
      (List.map
         (fun q -> (q.Query.title, q.debug_info.dbg_execution_path))
         queries ) ;
    let queries = queries |> List.map (fun q -> (q, query_output_file q)) in
    let fmts =
      queries |> List.map snd |> List.sort_uniq compare
      |> List.map (fun f -> (f, formatter_of_file (Tql_output.file f)))
    in
    let queries = queries |> List.map (fun (q, f) -> (q, List.assoc f fmts)) in
    fmts |> List.iter (fun (_, fmt) -> Format.fprintf fmt "@[<v>") ;
    let (module Evaluator : Query_evaluator.S) =
      if !use_legacy_evaluator then (module Query_eval_legacy)
      else (module Query_eval)
    in
    Evaluator.eval_batch ~trace_file:!trace_file queries ;
    fmts |> List.iter (fun (_, fmt) -> Format.fprintf fmt "@]@.") ;
    let emoji = if !no_color then "" else " \u{1F389}" in
    print_endline_styled [green] ("Done!" ^ emoji)

let () =
  try main () ; exit 0 with
  (* Normal user error *)
  | Tql_error.User_error e ->
      print_endline_styled [red] (Tql_error.string_of_error e) ;
      exit 1
  (* Internal errors *)
  | (Failure _ | Assert_failure _) as exn ->
      Log.error "A top-level exception was caught." ~exn ;
      exit 2
  | ExceptionDefn.Malformed_Decl _ as exn ->
      Log.error "A KaSim internal exception was raised." ~exn ;
      exit 2
