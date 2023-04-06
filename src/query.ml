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

(* Expressions *)

module AgentSet = Agent.SetMap.Set

type agent_set = AgentSet.t

type (_, _, _) binop =
  | Eq : ('a, 'a, bool) binop
  | Binop : ('a -> 'b -> 'c) -> ('a, 'b, 'c) binop
  | Concat : ('a, 'b, tuple) binop

and (_, _) unop =
  | Unop : ('a -> 'b) -> ('a, 'b) unop
  | Count_agents : agent_kind list -> (agent_set, tuple) unop

and _ expr_type =
  | Bool : bool expr_type
  | Int : int expr_type
  | Float : float expr_type
  | String : string expr_type
  | Agent_set : agent_set expr_type
  | Tuple : tuple expr_type

and value = Val : 'a * 'a expr_type -> value

and tuple = value list

type 'a expr = 'a expr_body * 'a expr_type

and _ expr_body =
  | Const : 'a -> 'a expr_body
  | Measure : (local_event_id * measure_id) -> 'a expr_body
  | Binop : 'a expr * ('a, 'b, 'c) binop * 'b expr -> 'c expr_body
  | Unop : ('a, 'b) unop * 'a expr -> 'b expr_body
  | Agent_id : local_agent_id -> int expr_body

(* Measures *)

type _ event_measure =
  | Time : float event_measure
  | Rule : string event_measure
  | Debug_event : string event_measure
  | Init_event : bool event_measure

type _ state_measure =
  | Int_state : (Agent.t * site_id) -> string state_measure
  | Count : pattern -> int state_measure
  | Component : local_agent_id -> agent_set state_measure
  | Nphos : local_agent_id -> int state_measure
  | Snapshot : string state_measure
  | Print_cc : local_agent_id -> string state_measure

type measure = {used_in_pattern: bool; measure_descr: measure_descr}

and measure_descr =
  | State_measure :
      state_measure_time * 'a expr_type * 'a state_measure
      -> measure_descr
  | Event_measure : 'a expr_type * 'a event_measure -> measure_descr

and state_measure_time = Before | After

(* Trace patterns *)

type rule_constraint =
  | Init
  | End_of_trace (* Not supported yet *)
  | Rule of int list
  | Obs of string

type event_pattern =
  { main_pattern: pattern
  ; with_clause: bool expr
        (* This is unsupported by the engine for now. When-clauses are
           available but they do not work the same way: they are just
           syntactic sugar for adding conditionals in actions. *)
  ; rule_constraint: rule_constraint option }

type defining_relation =
  | First_after of local_event_id * event_pattern
  | Last_before of local_event_id * event_pattern

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
  ; measures: measure array
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

type ('a, 'b) tree = Tree of 'a * ('b * ('a, 'b) tree) list

(* A tree that tells in what order events should be discovered. The root
   of this tree corresponds to the root of the trace pattern. *)
type matching_tree = (local_event_id, defining_relation) tree

type trace_pattern =
  { agents: agent_kind array (* Question: all agents or some? *)
  ; events: event array (* Indexed by [local_event_id] *)
  ; traversal_tree: matching_tree }

(* TODO: add equality_constraints: (event_id * event_id) list ; *)

type action = Print : 'a expr -> action | If : bool expr * action -> action

type query =
  { title: string option
  ; legend: string list option (* Used as headers for the resulting CSV file. *)
  ; pattern: trace_pattern
  ; action: action
  ; every_clause: float option }
