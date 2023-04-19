(*****************************************************************************)
(* New query evaluator                                                       *)
(*****************************************************************************)

(* A query is evaluated in several steps:
     1. We first cache the results of [Event_matcher.match_event] for
        each event in the trace.
     2. Then, we compute all matchings.
     3. We prepare a measure plan as a [(step_id, matching_id,
        event_id))] array.
     4. We execute the measure plan

   For "simple" queries (i.e. queries with a single event clause), a
   simpler, faster, one-step execution strategy is used.
*)

open Dump

(*****************************************************************************)
(* Utilities                                                                 *)
(*****************************************************************************)

let number_of_events q = Array.length q.Query.pattern.events

let number_of_agents q = Array.length q.Query.pattern.agents

let query_root_event q = List.hd q.Query.pattern.execution_path

(*****************************************************************************)
(* Link cache computation                                                    *)
(*****************************************************************************)

(* A "link" is an ordered matching of all agents in [event.link_agents] *)
module Link = struct
  type t = Aliases.global_agent_id list [@@deriving show, yojson]

  let pp = Fmt.(list int)

  let compare v v' =
    assert (List.length v = List.length v') ;
    compare v v'
end

module LinkMap = Utils.MapV2.Make (Link)

module LinkCache = struct
  (* For every event [ev], we map a matching of [ev.link_agents] (i.e. a
     link) to a sequence of [(step_id_in_trace, potential_matching)]
     pairs. The sequence is represented using a [History.t]
     datastructure for efficient access. *)
  type t = Cache of Event_matcher.status History.t LinkMap.t array
  [@@deriving show, yojson_of]

  let create query =
    Cache (Array.init (number_of_events query) (fun _ -> LinkMap.empty))

  let add (Cache cache) ~ev_lid ~ev_gid pot_match =
    let ev_cache = cache.(ev_lid) in
    let link = pot_match.Event_matcher.link in
    let hist =
      LinkMap.find_opt link ev_cache
      |> Option.value ~default:History.empty
      |> History.add ev_gid pot_match.status
    in
    cache.(ev_lid) <- LinkMap.add link hist ev_cache

  let access_cache f cache ev_lid link ref_ev_gid =
    let ev_cache = cache.(ev_lid) in
    match LinkMap.find_opt link ev_cache with
    | None ->
        None
    | Some hist ->
        f ref_ev_gid hist

  let first_after cache = access_cache History.first_after cache

  let last_before cache = access_cache History.last_before cache

  let dump query (Cache cache) : Yojson.Safe.t =
    `Assoc
      ( Array.to_list cache
      |> List.mapi (fun ev_lid ev_cache ->
             let ev = query.Query.pattern.events.(ev_lid) in
             ( dump_ev_lid query ~lid:ev_lid
             , `Assoc
                 ( LinkMap.bindings ev_cache
                 |> List.map (fun (link, hist) ->
                        ( dump_link query ev link
                        , `Assoc
                            ( History.to_alist hist
                            |> List.map (fun (ev_gid, status) ->
                                   ( string_of_int ev_gid
                                   , `String (dump_status query ev status) ) )
                            ) ) ) ) ) ) )
end

let compute_link_cache_step query window cache =
  Array.iteri
    (fun ev_lid ev ->
      match Event_matcher.match_event ev window with
      | None ->
          ()
      | Some pot_match ->
          LinkCache.add cache ~ev_lid ~ev_gid:window.step_id pot_match )
    query.Query.pattern.events

(*****************************************************************************)
(* Enumerate all matchings                                                   *)
(*****************************************************************************)

(* Arrays indexed by [local_event_id] and [local_agent_id] respectively *)
type matching =
  | M of {evs: Aliases.global_event_id array; ags: Aliases.global_agent_id array}
[@@deriving show, yojson_of]

let dump_matching query (M {evs; ags}) =
  let indexes t = Array.mapi (fun i _ -> i) t in
  let evs =
    dump_events_mapping_list query
      ~lids:(Array.to_list (indexes query.Query.pattern.events))
      ~gids:(Array.to_list evs)
  in
  let ags =
    dump_agents_mapping_list query
      ~lids:(Array.to_list (indexes query.Query.pattern.agents))
      ~gids:(Array.to_list ags)
  in
  Fmt.str "%s | %s" evs ags

let dump_all_matchings q ms : Yojson.Safe.t =
  `List (List.map (fun m -> `String (dump_matching q m)) (Array.to_list ms))

type root_matching =
  | RM of
      {ev: Aliases.global_event_id; constrained: Aliases.global_agent_id list}
[@@deriving show, yojson_of]

(* Compute the list of all matchings of the root event *)
let compute_root_matchings query (LinkCache.Cache link_cache) =
  let root_id = query_root_event query in
  match LinkMap.bindings link_cache.(root_id) with
  | [] ->
      []
  | [([], hist)] ->
      (* We have [root_event.link_agents = []] so the map must have a
         unique binding at most *)
      History.to_alist hist
      |> List.map (fun (step_id, status) ->
             match status with
             | Event_matcher.Failure ->
                 (* The root event has no defining relation pattern so a
                    potential matching cannot fail. *)
                 assert false
             | Success {other_constrained} ->
                 RM {ev= step_id; constrained= other_constrained} )
  | _ ->
      assert false

(* This exception is raised within [complete_matching] when an
   inconsistency is detected and therefore the partial matching must be
   thrown away *)
exception No_match

type agent_matching_list =
  | AML of (Aliases.local_agent_id * Aliases.global_agent_id) list

type partial_matching_delta =
  | PMD of {ev_lid: int; ev_gid: int; eqs: agent_matching_list}

(* In a partial matching, non-matched ids are mapped to -1. *)
let update_partial_matching (M m) (PMD {ev_lid; ev_gid; eqs= AML eqs}) =
  m.evs.(ev_lid) <- ev_gid ;
  eqs
  |> List.iter (fun (lid, gid) ->
         if m.ags.(lid) <> -1 && m.ags.(lid) <> gid then raise No_match ;
         m.ags.(lid) <- gid )

let root_delta (RM root) ~ev_lid ev =
  let eqs =
    AML (Utils.list_zip ev.Query.other_constrained_agents root.constrained)
  in
  PMD {ev_lid; ev_gid= root.ev; eqs}

let nonroot_delta (LinkCache.Cache link_cache) (M m) ~ev_lid ev =
  let ref_ev_lid, access_cache =
    match ev.Query.defining_rel with
    | None ->
        assert false (* A nonroot event must have a defining relation *)
    | Some (First_after (ref_ev_lid, _)) ->
        (ref_ev_lid, LinkCache.first_after)
    | Some (Last_before (ref_ev_lid, _)) ->
        (ref_ev_lid, LinkCache.last_before)
  in
  let ref_ev_gid = m.evs.(ref_ev_lid) in
  assert (ref_ev_gid <> -1) ;
  let link = List.map (fun i -> m.ags.(i)) ev.link_agents in
  match access_cache link_cache ev_lid link ref_ev_gid with
  | None | Some (_, Event_matcher.Failure) ->
      raise No_match
  | Some (ev_gid, Success {other_constrained}) ->
      let eqs =
        AML
          ( Utils.list_zip ev.link_agents link
          @ Utils.list_zip ev.other_constrained_agents other_constrained )
      in
      PMD {ev_lid; ev_gid; eqs}

let complete_matching query link_cache (RM root) =
  let evs = Array.make (number_of_events query) (-1) in
  let ags = Array.make (number_of_agents query) (-1) in
  let m = M {evs; ags} in
  try
    query.pattern.execution_path
    |> List.iter (fun ev_lid ->
           let ev = query.pattern.events.(ev_lid) in
           let delta =
             if ev_lid = query_root_event query then
               root_delta (RM root) ~ev_lid ev
             else nonroot_delta link_cache m ~ev_lid ev
           in
           update_partial_matching m delta ) ;
    Some (M {evs; ags})
  with No_match -> None

let compute_all_matchings query link_cache =
  compute_root_matchings query link_cache
  |> List.filter_map (complete_matching query link_cache)
  |> Array.of_list

(*****************************************************************************)
(* Preparing measurement schedule                                            *)
(*****************************************************************************)

type measurement_schedule_item = {ev_gid: int; matching_id: int; ev_lid: int}
[@@deriving yojson_of]

(* We use an array instead of a list to save memory *)
type measurement_schedule = measurement_schedule_item array
[@@deriving yojson_of]

let measurement_schedule matchings =
  let items = Queue.create () in
  Array.iteri
    (fun matching_id (M {evs; _}) ->
      Array.iteri
        (fun ev_lid ev_gid -> Queue.add {ev_gid; matching_id; ev_lid} items)
        evs )
    matchings ;
  let items = items |> Queue.to_seq |> Array.of_seq in
  Array.fast_sort (fun {ev_gid= e; _} {ev_gid= e'; _} -> compare e e') items ;
  items

(*****************************************************************************)
(* Executing measurement schedule                                            *)
(*****************************************************************************)

type measure_cache_item =
  { mutable measures: Value.t array array
        (* Maps [(event_id, measure_id)] to a value. To save memory,
           this is initialinzed to the empty array. The first time a value
           is cached, an array with proper dimension is created. It is
           reset to the empty array after the matching's action is executed
           to save memory. *)
  ; mutable rem: int
        (* Number of events in the matchings that remain to be
           processed. When this number drops to zero, the action can be
           executed *) }

type measurement_env =
  { cache: measure_cache_item array (* indexed by matching id *)
  ; schedule: measurement_schedule
  ; mutable cur: int (* index of the current plan step in [schedule] *) }

let create_measures_cache matchings =
  Array.map
    (fun (M {evs; _}) ->
      let rem = Array.length evs in
      let measures = [||] in
      {measures; rem} )
    matchings

let create_measurement_env matchings =
  { cache= create_measures_cache matchings
  ; schedule= measurement_schedule matchings
  ; cur= 0 }

let rec execute_action ~fmt ~read_measure ~read_id = function
  | Query.Print e ->
      Expr.eval_expr read_measure read_id e
      |> fun v -> Format.fprintf fmt "%s@;" (Value.to_string v)
  | If (cond, action) -> (
      let b = Expr.eval_expr read_measure read_id cond in
      match Value.(cast TBool b) with
      | None ->
          Tql_error.failwith "The 'when' clause was passed a non-boolean value"
      | Some b ->
          if b then execute_action ~fmt ~read_measure ~read_id action )

let perform_measurement_step ~header ~fmt query env matchings window =
  while
    env.cur < Array.length env.schedule
    && env.schedule.(env.cur).ev_gid = window.Streaming.step_id
  do
    let {matching_id; ev_lid; _} = env.schedule.(env.cur) in
    let (M matching) = matchings.(matching_id) in
    let cache = env.cache.(matching_id) in
    (* Initialize the measure cache if needed *)
    if Array.length cache.measures = 0 then
      cache.measures <-
        Array.map
          (fun ev -> Array.make (Array.length ev.Query.measures) Value.VNull)
          query.Query.pattern.events ;
    (* We perform all measurements *)
    Array.iteri
      (fun measure_id mdescr ->
        cache.measures.(ev_lid).(measure_id) <-
          Measure.take_measure ~header
            (fun lid -> matching.ags.(lid))
            window mdescr.Query.measure )
      query.Query.pattern.events.(ev_lid).measures ;
    (* Possibly execute the action *)
    cache.rem <- cache.rem - 1 ;
    if cache.rem <= 0 then (
      (* Perform the action *)
      execute_action ~fmt
        ~read_id:(fun lid -> matching.ags.(lid))
        ~read_measure:(fun ev_lid m_id -> cache.measures.(ev_lid).(m_id))
        query.Query.action ;
      (* Free the measurements' memory *)
      cache.measures <- [||] ) ;
    (* Execute the next step of the measurement plan *)
    env.cur <- env.cur + 1
  done

(*****************************************************************************)
(* Execute simple queries                                                    *)
(*****************************************************************************)

type simple_engine_state = {mutable last_action_time: float}

let init_simple_engine_state () = {last_action_time= neg_infinity}

let enough_time_elapsed query state t =
  match query.Query.every_clause with
  | None ->
      true
  | Some delta ->
      t -. state.last_action_time >= delta

let perform_measurement_step_for_simple_query ~header ~fmt query state window =
  let t = Safe_replay.time window.Streaming.state in
  if enough_time_elapsed query state t then
    let event = query.Query.pattern.events.(0) in
    match Event_matcher.match_event event window with
    | None ->
        ()
    | Some {status= Failure; _} ->
        assert false (* No defining clauses for simple queries *)
    | Some {status= Success {other_constrained}; _} ->
        let ag_matching =
          Utils.list_zip event.other_constrained_agents other_constrained
        in
        let read_id lid = List.assoc lid ag_matching in
        execute_action ~fmt ~read_id
          ~read_measure:(fun _ m_id ->
            Measure.take_measure ~header read_id window
              event.measures.(m_id).measure )
          query.Query.action ;
        state.last_action_time <- t

(*****************************************************************************)
(* Main function                                                             *)
(*****************************************************************************)

let iter_trace ~trace_file f =
  Streaming.fold_trace ~update_ccs:true ~compute_previous_states:true
    ~skip_init_events:false ~trace_file
    (fun w () -> f w)
    ()

let dump_trace_full ~trace_file =
  let q = Queue.create () in
  let header = Trace_header.load ~trace_file in
  iter_trace ~trace_file (fun w ->
      Queue.push
        (string_of_int w.step_id, Trace_util.dump_step header.model w.step)
        q ) ;
  `Assoc (Utils.list_of_queue q)

let dump_trace ~trace_file =
  let open Trace_util in
  let q = Queue.create () in
  let header = Trace_header.load ~trace_file in
  iter_trace ~trace_file (fun w ->
      Queue.push
        ( string_of_int w.step_id
        , `List
            [ `String (rule_name header.model w.step)
            ; `String (dump_step_actions header.model w.step) ] )
        q ) ;
  `Assoc (Utils.list_of_queue q)

let map_queries qs f =
  Array.mapi
    (fun i (q, fmt) ->
      Log.with_current_query q.Query.title (fun () -> f i q fmt) )
    qs

let with_queries qs f = ignore (map_queries qs f)

let batch_dump ~level file queries f =
  Tql_output.debug_json ~level file (fun () ->
      `Assoc
        ( Array.mapi (fun i (q, _) -> (q.Query.title, f i q)) queries
        |> Array.to_list ) )

let print_legend (q, fmt) =
  match q.Query.legend with
  | None ->
      ()
  | Some ls ->
      let labels = List.map (fun l -> "'" ^ l ^ "'") ls in
      Format.fprintf fmt "%s@;" (String.concat ", " labels)

let make_progress_bar () =
  Terminal.open_progress_bar ~step:10_000 ~info:(fun i ->
      Fmt.str "%.2fM events processed" (float_of_int i /. 1e6) )

let eval_batch ~trace_file queries_and_formatters =
  let header = Trace_header.load ~trace_file in
  List.iter print_legend queries_and_formatters ;
  (* Split queries into simple and complex queries *)
  let simple, complex =
    queries_and_formatters
    |> List.partition (fun (q, _) -> Query.is_simple q)
    |> fun (l, l') -> (Array.of_list l, Array.of_list l')
  in
  Terminal.(
    println [cyan; bold]
      (Fmt.str "Evaluating queries: %d simple and %d complex"
         (Array.length simple) (Array.length complex) ) ) ;
  (* Dump a summary of the trace along with query execution paths *)
  Tql_output.debug_json ~level:2 "trace-summary.json" (fun () ->
      dump_trace ~trace_file ) ;
  Tql_output.debug_json ~level:2 "trace-summary-long.json" (fun () ->
      dump_trace_full ~trace_file ) ;
  batch_dump ~level:1 "execution-paths.json" complex (fun _ q ->
      `String (dump_execution_path q q.Query.pattern.execution_path) ) ;
  (* First pass through the trace (for complex queries only) *)
  let complex_matchings =
    if Array.length complex = 0 then [||]
    else
      let caches = Array.map (fun (q, _) -> LinkCache.create q) complex in
      let bar = make_progress_bar () in
      Terminal.with_progress_bar "Filling in the history cache" bar
        (fun ~progress ->
          iter_trace ~trace_file (fun w ->
              with_queries complex (fun i q _ ->
                  compute_link_cache_step q w caches.(i) ) ;
              progress 1 ) ) ;
      batch_dump ~level:2 "link-cache.json" complex (fun i q ->
          LinkCache.dump q caches.(i) ) ;
      let matchings =
        Terminal.task "Computing matchings" (fun () ->
            map_queries complex (fun i q _ ->
                compute_all_matchings q caches.(i) ) )
      in
      batch_dump ~level:2 "matchings.json" complex (fun i q ->
          dump_all_matchings q matchings.(i) ) ;
      matchings
  in
  (* Second pass through the trace *)
  let complex_envs = Array.map create_measurement_env complex_matchings in
  let simple_envs = Array.map (fun _ -> init_simple_engine_state ()) simple in
  batch_dump ~level:2 "measurement-schedule.json" complex (fun i _q ->
      [%yojson_of: measurement_schedule] complex_envs.(i).schedule ) ;
  let bar = make_progress_bar () in
  Terminal.with_progress_bar "Executing actions" bar (fun ~progress ->
      iter_trace ~trace_file (fun w ->
          with_queries complex (fun i q fmt ->
              perform_measurement_step ~header ~fmt q complex_envs.(i)
                complex_matchings.(i) w ) ;
          with_queries simple (fun i q fmt ->
              perform_measurement_step_for_simple_query ~header ~fmt q
                simple_envs.(i) w ) ;
          progress 1 ) )
