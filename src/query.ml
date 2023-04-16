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

type pat_agent_spec = {pat_agent_kind: agent_kind} [@@deriving show, yojson]

type pattern =
  { agents: pat_agent_spec array (* Agents are indexed by their [pat_agent_id] *)
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
  ; event_name: string option (* For debugging purposes *)
  ; event_pattern: event_pattern option
  ; defining_rel: defining_relation option
  ; measures: measure_descr array
        (* During compilation, measures are extracted from expressions
           and given an ID ([measure_id]) corresponding to their index
           in this array. *)
  ; link_agents: local_agent_id list
  ; other_constrained_agents: local_agent_id list
        (* The two fields above are computed with the [execution_path].
            They form a partition of the set of constrained agents (i.e.
            the set of [local_agent_id] that are constrained in either
            the main pattern or the defining pattern). A local agent is
            part of [link_agents] iff (1) it is constrained in the
            defining pattern and (2) it is constrained by at least one
            previous event in the execution path. To motivate the
            terminology of "link agent", let us consider an example
            of a query and how it is evaluated:

                match e:{r:..., s:...}
                and last f:{r:..., u:...} before e
                and f:{r:..., s:..., t:...}
                and g:{t: ...}

           To evaluate this query, one first considers all possible
           mappings of e in the trace (each mapping determines the
           identity of r and s). Given a particular mapping, the only
           possible identity of f can be determined using the query's
           second clause (i.e. the defining pattern of f). To find the
           candidate for f, one must look at the set of all events in
           the trace that match the pattern from the clause for a fixed
           value of r. We have [link_agents = [r]]. u is not an
           link agent since it is not pinned already and s, t are
           not either since they are not constrained in the defining
           pattern of f. We have [other_constrained_agents = [u, s, t]].
           Finally, the only link agent for g is t.
        *) }
[@@deriving show, yojson_of]

type execution_path = local_event_id list [@@deriving show, yojson_of]
(* A topological sorting of the query's dependency graph. The first
   event from this list is called the "root". Mapping the root to an
   event in the trace must be sufficient to determine an unambiguous
   mapping for all other events (this is called "local rigidity" in the
   paper). Once the root is "pinned", the mapping of other events is
   deduced in the order specified by the execution path. *)

type agent_spec =
  {agent_kind: agent_kind; agent_name: string (* For debugging purposes *)}
[@@deriving show, yojson_of]

type trace_pattern =
  { agents: agent_spec array
        (* all constrained agents, indexed by [local_agent_id] *)
  ; events: event array (* indexed by [local_event_id] *)
  ; execution_path: execution_path }
[@@deriving show, yojson_of]

type action = Print of Expr.t | If of Expr.t * action
[@@deriving show, yojson_of]

type t =
  { title: string
  ; legend: string list option (* Used as headers for the resulting CSV file. *)
  ; pattern: trace_pattern
  ; action: action
  ; every_clause: float option }
[@@deriving show, yojson_of]

let is_simple q = Array.length q.pattern.events = 1
