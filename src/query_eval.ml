(*****************************************************************************)
(* New query evaluator                                                       *)
(*****************************************************************************)

(* A query is evaluated in several steps:
    1. We first cache the results of [Event_matcher.match_event] for
       each event in the trace.
    2. Then, we compute all matchings.
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
  query.Query.pattern.events
  |> Array.iteri (fun ev_lid ev ->
         match Event_matcher.match_event ev window with
         | None ->
             ()
         | Some pot_match ->
             LinkCache.add cache ~ev_lid ~ev_gid:window.step_id pot_match )

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
  assert (ref_ev_gid <> 1) ;
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
(* Main function                                                             *)
(*****************************************************************************)

let iter_trace ~trace_file f =
  Streaming.fold_trace ~update_ccs:true ~compute_previous_states:true
    ~skip_init_events:false ~trace_file
    (fun w () -> f w)
    ()

let dump_trace ~trace_file =
  let open Streaming in
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

let batch_iter_trace ~trace_file ~queries f =
  iter_trace ~trace_file (fun w ->
      Array.iteri
        (fun i q -> Log.with_current_query q.Query.title (fun () -> f i q w))
        queries )

let batch_dump ~level file ~queries f =
  Tql_output.debug_json ~level file (fun () ->
      `Assoc
        (Array.mapi (fun i q -> (q.Query.title, f i q)) queries |> Array.to_list) )

let eval_batch ~trace_file queries_and_formatters =
  Tql_output.debug_json ~level:2 "trace-summary.json" (fun () ->
      dump_trace ~trace_file ) ;
  let queries = Array.map fst (Array.of_list queries_and_formatters) in
  batch_dump ~level:1 "execution-paths.json" ~queries (fun _ q ->
      `String (dump_execution_path q q.Query.pattern.execution_path) ) ;
  let _formatters = Array.map snd (Array.of_list queries_and_formatters) in
  let caches = Array.map LinkCache.create queries in
  batch_iter_trace ~trace_file ~queries (fun i q w ->
      compute_link_cache_step q w caches.(i) ) ;
  batch_dump ~level:2 "link-cache.json" ~queries (fun i q ->
      LinkCache.dump q caches.(i) ) ;
  let matchings = Array.map2 compute_all_matchings queries caches in
  batch_dump ~level:2 "matchings.json" ~queries (fun i q ->
      dump_all_matchings q matchings.(i) )
