(*****************************************************************************)
(* KaTie: The Kappa Trace Inquiry Engine                                     *)
(*****************************************************************************)

open Ppx_yojson_conv_lib.Yojson_conv

let trace_file = ref ""

let query_file = ref ""

let no_backtraces = ref false

let snapshots_name_format = ref ""

let skip_invalid = ref false

let ccs_incremental = ref false

let eval_only = ref None

(* state variables *)

let execution_started =
  ref false (* used to differentiate static and dynamic errors *)

let some_queries_are_invalid = ref false
(* set to true if some queries are invalid and were skipped *)

let native_snapshots () = Output.snapshots_native_format := true

let usage = Sys.argv.(0) ^ " queries a Kappa trace"

let options =
  [ ("-q", Arg.Set_string query_file, "query file")
  ; ("-t", Arg.Set_string trace_file, "trace file")
  ; ( "--only"
    , Arg.String (fun s -> eval_only := Some s)
    , "only evaluate a subset of queries (expects a comma-separated list of \
       names)" )
  ; ( "--output-dir"
    , Arg.String Output.set_output_directory
    , "set the output directory (default: '.')" )
  ; ( "--debug-level"
    , Arg.Set_int Output.debug_level
    , "set the debug level (0, 1, 2) (default: 1)" )
  ; ( "--profile"
    , Arg.Set Profile.profiling_enabled
    , "output a 'profile.json' file with profiling info" )
  ; ("--no-backtraces", Arg.Set no_backtraces, "disable exception backtraces")
  ; ( "--snapshots-names"
    , Arg.Set_string snapshots_name_format
    , "name format of generated snapshot files (default: 'snapshot.%.json' or \
       'snapshot.%.ka')" )
  ; ( "--native-snapshots"
    , Arg.Unit native_snapshots
    , "dump snapshot using KaSim's native format" )
  ; ("--no-color", Arg.Set Terminal.no_color, "disable colored output")
  ; ( "--ccs-incremental"
    , Arg.Set ccs_incremental
    , "compute all connected components incrementally (unadvised)" )
  ; ( "--no-progress-bars"
    , Arg.Set Terminal.disable_progress_bars
    , "disable progress bars to avoid polluting stdout" )
  ; ( "--skip-invalid"
    , Arg.Set skip_invalid
    , "skip invalid queries while outputting an 'errors.json' file" ) ]

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

let compile_and_check_all model queries =
  Sanity_checks.check_unique_names queries ;
  let errors = Queue.create () in
  let queries =
    List.filter_map
      (fun q ->
        try Some (compile_and_check model q)
        with Error.User_error e as exn ->
          if not !skip_invalid then raise exn ;
          Queue.add (q.query_name, [%yojson_of: Error.error_kind] e.kind) errors ;
          None )
      queries
  in
  if not (Queue.is_empty errors) then some_queries_are_invalid := true ;
  if !skip_invalid then
    Output.write_json "errors.json" (`Assoc (Utils.list_of_queue errors)) ;
  queries

let dump_profiling_info () =
  if !Profile.profiling_enabled then
    Output.write_json "profile.json" (Profile.dump_json ())

let formatter_of_file f = Format.formatter_of_out_channel (open_out f)

let filter_asts filter asts =
  match filter with
  | None ->
      asts
  | Some only ->
      let module Sset = Set.Make (String) in
      let only = String.split_on_char ',' only |> Sset.of_list in
      let available =
        List.map (fun q -> q.Query_ast.query_name) asts |> Sset.of_list
      in
      let missing = Sset.diff only available in
      ( if not (Sset.is_empty missing) then
          let missing =
            missing |> Sset.to_seq |> List.of_seq
            |> List.map (fun s -> "'" ^ s ^ "'")
            |> String.concat ", "
          in
          Error.failwith ("No queries with names: " ^ missing) ) ;
      List.filter (fun q -> Sset.mem q.Query_ast.query_name only) asts

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
    Log.set_global_model header.model ;
    let asts = parse_queries !query_file in
    Output.debug_json "queries-ast.json" (fun () ->
        [%yojson_of: Query_ast.t list] asts ) ;
    let asts = filter_asts !eval_only asts in
    let queries = compile_and_check_all header.model asts in
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
    execution_started := true ;
    Query_eval.eval_batch ~ccs_incremental:!ccs_incremental
      ~trace_file:!trace_file queries_and_formatters ;
    List.iter (fun (_, fmt) -> Format.fprintf fmt "@]@.") queries_and_formatters ;
    Terminal.(println [bold; green] "Done.")

let () =
  try
    Sys.catch_break true (* Catch the Ctrl+C signal *) ;
    main () ;
    dump_profiling_info () ;
    exit (if !some_queries_are_invalid then 1 else 0)
  with
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
  | ExceptionDefn.Malformed_Decl msg ->
      Terminal.(println [red] ("KaSim error: " ^ fst msg)) ;
      exit 1
  (* Internal errors *)
  | (Failure _ | Assert_failure _) as exn ->
      Log.error "A top-level exception was caught." ~exn ;
      exit 3
  | Sys.Break ->
      dump_profiling_info () ;
      (* newline in case there is a progress bar running *)
      Terminal.println [] "" ;
      Terminal.(println [red] "Terminated by user.") ;
      exit 1
