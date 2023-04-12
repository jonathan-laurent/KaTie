(*****************************************************************************)
(* Compiled query                                                            *)
(*****************************************************************************)

open Aliases

type lnk_state =
  | Free
  | Bound_to of pat_site
  | Bound_to_type of (agent_kind * site_id)
  | Bound_to_any
[@@deriving show, yojson]

type test =
  | Agent_exists of local_agent_id
  | Lnk_state_is of local_site * lnk_state
  | Int_state_is of local_site * int_state
[@@deriving show, yojson]

type modification =
  | Create of local_agent_id
  | Destroy of local_agent_id
  | Mod_int_state of local_site * int_state
  | Mod_lnk_state of local_site * lnk_state
[@@deriving show, yojson]

type agent_spec = {agent_kind: agent_kind} [@@deriving show, yojson]

type pattern =
  { agents: agent_spec array (* Agents are indexed by their [pat_agent_id] *)
  ; tests: test list
  ; mods: modification list
  ; agent_constraints: pat_agent_id local_agent_id_map
        (* Maps [local_agent_ids] that are constrained in this pattern
           to [pat_agent_ids]. For example, if 'k' is compiled to local
           agent id 7, then in event { S(d[/1]), k:K(d[/1]) },
           [agent_constraints: 7 -> 1] (since 'k' names the second agent
           in the pattern) *)
        (* rename [constrained_agents]? *) }
[@@deriving show, yojson_of]

(* Trace patterns *)

type rule_constraint =
  | Init
  | End_of_trace (* Not supported yet *)
  | Rule of int list
  | Obs of string
[@@deriving show, yojson_of]

type event_pattern =
  { main_pattern: pattern
  ; with_clause: Expr.t option
        (* This is unsupported by the engine for now. When-clauses are
           available but they do not work the same way: they are just
           syntactic sugar for adding conditionals in actions. *)
  ; rule_constraint: rule_constraint option }
[@@deriving show, yojson_of]

type defining_relation =
  | First_after of local_event_id * event_pattern
  | Last_before of local_event_id * event_pattern
[@@deriving show, yojson_of]

type measure_descr = {measure: Measure.t; used_in_pattern: bool}
[@@deriving show, yojson_of]

(* Each event is associated to either one or two patterns. The pattern
   in [event.defining_rel] is mandatory for all events except the root.
   It is used to uniquely map events in the trace from the mapping of
   its predecessors in the dependency graph. For non-root nodes,
   [event.event_pattern] can define additional constraints and constrain
   additional agents. For example, in the example below:

       match b:{ S(d[/1]), k:K(d[/1]) }
       and first u:{ k:K(d[/.]) } after b
       and u:{ k:K(x{p}) }
       return (time[u] - time[b])

   local event 'b' has one 'event_pattern' and local event 'b' has both
   a 'defining_rel' and an 'event_pattern'. Right now, the engine does
   not handle such a query though since it would try and match u's
   nondefining pattern to a trace step without context and find
   ambiguitites (since it does not feature any action). In the future,
   it is debatable whether we should improve this or just drop support
   for having several patterns for actions altogether (most use-cases
   for this can be handled by reformulating the query or using
   'when'-clauses anyway.). *)
type event =
  { event_id: local_event_id (* Index in query.pattern.events *)
  ; event_pattern: event_pattern option
  ; defining_rel: defining_relation option
  ; measures: measure_descr array
        (* During compilation, measures are extracted from expressions
           and given an ID ([measure_id]) corresponding to their index
           in this array. *)
  ; already_constrained_agents: local_agent_id list
  ; captured_agents: local_agent_id list
        (* The two fields above are computed with the traversal tree.
           They partition the set of [local_agent_id] constrained by the
           event (a local agent is constrained if it occurs either in
           the [defining_rel] pattern or in the [event_pattern]) into
           two parts. [already_constrained_agents] are local_agents that
           will have been already mapped to global agents in the trace
           when the event is attempted to be matched. [captured_agents]
           are agents whose identity is to be figured out by matching
           this specific event. *)
        (* We further assume that the
           [defining_rel] pattern can only feature already constrained
           agents. TODO: why this assumption? Do we need it? *) }
[@@deriving show, yojson_of]

type ('a, 'b) tree = Tree of 'a * ('b * ('a, 'b) tree) list
[@@deriving show, yojson_of]

(* A tree that tells in what order events should be discovered. The root
   of this tree corresponds to the root of the trace pattern. *)
type matching_tree = (local_event_id, defining_relation) tree
[@@deriving show, yojson_of]

type trace_pattern =
  { agents: agent_kind array (* Question: all agents or some? *)
  ; events: event array (* Indexed by [local_event_id] *)
  ; traversal_tree: matching_tree }
[@@deriving show, yojson_of]

(* TODO: add equality_constraints: (event_id * event_id) list ; *)

type action = Print of Expr.t | If of Expr.t * action
[@@deriving show, yojson_of]

type query =
  { title: string option
  ; legend: string list option (* Used as headers for the resulting CSV file. *)
  ; pattern: trace_pattern
  ; action: action
  ; every_clause: float option }
[@@deriving show, yojson_of]
