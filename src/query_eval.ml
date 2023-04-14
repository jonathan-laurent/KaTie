(*****************************************************************************)
(* New query evaluator                                                       *)
(*****************************************************************************)

open Aliases
open Query

(* A query is evaluated in several steps:
    1. We first cache the results of [Event_matcher.match_event] for
       each event in the trace.
    2. Then, we compute all matchings.
*)

(*****************************************************************************)
(* Utilities                                                                 *)
(*****************************************************************************)

let number_of_events q = Array.length q.pattern.events

let number_of_agents q = Array.length q.pattern.agents

let query_root_event q = List.hd q.pattern.execution_path

(*****************************************************************************)
(* Link cache computation                                                    *)
(*****************************************************************************)

(* A "link" is an ordered matching of all agents in [event.link_agents] *)
module Link = struct
  type t = global_agent_id list [@@deriving show, yojson]

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
  type t =
    | Cache of Event_matcher.status History.t LinkMap.t array
        [@deriving show, yojson_of]

  let create query =
    Cache (Array.init (number_of_events query) (fun _ -> LinkMap.empty))

  let add (Cache cache) ev_id step_id_in_trace pot_match =
    let ev_cache = cache.(ev_id) in
    let link = pot_match.Event_matcher.link in
    let hist =
      LinkMap.find_opt link ev_cache
      |> Option.value ~default:History.empty
      |> History.add step_id_in_trace pot_match.status
    in
    cache.(ev_id) <- LinkMap.add link hist ev_cache

  let access_cache f cache ev_id link ref_step_id =
    let ev_cache = cache.(ev_id) in
    match LinkMap.find_opt link ev_cache with
    | None ->
        None
    | Some hist ->
        f ref_step_id hist

  let first_after cache = access_cache History.first_after cache

  let last_before cache = access_cache History.last_before cache
end

let compute_link_cache_step query window cache =
  query.pattern.events
  |> Array.iteri (fun ev_id ev ->
         match Event_matcher.match_event ev window with
         | None ->
             ()
         | Some pot_match ->
             LinkCache.add cache ev_id window.step_id pot_match )

(*****************************************************************************)
(* Enumerate all matchings                                                   *)
(*****************************************************************************)

(* Arrays indexed by [local_event_id] and [local_agent_id] respectively *)
type matching = M of {evs: global_event_id array; ags: global_agent_id array}
[@@deriving show, yojson_of]

type root_matching =
  | RM of {ev: global_event_id; constrained: global_agent_id list}
[@@deriving show, yojson_of]

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

exception No_match

let complete_matching query link_cache (RM root) : matching option =
  let evs = Array.make (number_of_events query) (-1) in
  let ags = Array.make (number_of_agents query) (-1) in
  let root_id = query_root_event query in
  try
    query.pattern.execution_path
    |> List.iter (fun ev_id ->
           let ev = query.pattern.events.(ev_id) in
           let step_id, eqs =
             if ev_id = root_id then
               (* For the root, we just use the provided root matching *)
               ( root.ev
               , Utils.list_zip ev.other_constrained_agents root.constrained )
             else
               (* For a non-root node, we use the link cache *)
               match ev.defining_rel with
               | None ->
                   assert
                     false (* A nonroot event must have a defining relation *)
               | Some (First_after (ref_ev_id, _)) -> (
                   let ref_ev_gid = evs.(ref_ev_id) in
                   assert (ref_ev_gid <> 1) ;
                   let link = List.map (fun i -> ags.(i)) ev.link_agents in
                   match
                     LinkCache.first_after link_cache ev_id link ref_ev_gid
                   with
                   | None | Some (_, Event_matcher.Failure) ->
                       raise No_match
                   | Some (ev_gid, Success {other_constrained}) ->
                       ( ev_gid
                       , Utils.list_zip ev.link_agents link
                         @ Utils.list_zip ev.other_constrained_agents
                             other_constrained ) )
               | Some (Last_before (_ref_ev_id, _)) ->
                   assert false
           in
           evs.(ev_id) <- step_id ;
           eqs
           |> List.iter (fun (lid, gid) ->
                  if ags.(lid) <> -1 && ags.(lid) <> gid then raise No_match ;
                  ags.(lid) <- gid ) ) ;
    Some (M {evs; ags})
  with No_match -> None

let compute_all_matchings query link_cache =
  compute_root_matchings query link_cache

(*****************************************************************************)
(* Main function                                                             *)
(*****************************************************************************)

let eval_batch ~trace_file:_ _qs = assert false
