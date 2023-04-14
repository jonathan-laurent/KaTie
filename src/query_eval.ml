(*****************************************************************************)
(* New query evaluator                                                       *)
(*****************************************************************************)

open Aliases

(* A query is evaluated in several steps:
    1. We first cache the results of [Event_matcher.match_event] for
       each nonroot event in the trace.
*)

(*****************************************************************************)
(* Link cache computation                                                    *)
(*****************************************************************************)

(* A "link" is an ordered matching of all agents in [event.link_agents] *)
module Link = struct
  type t = global_agent_id list [@@deriving yojson]

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

let eval_batch ~trace_file:_ _qs = assert false
