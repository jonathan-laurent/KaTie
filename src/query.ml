(*****************************************************************************)
(* Compiled query                                                            *)
(*****************************************************************************)

type agent_id = int [@@deriving show, yojson]

type event_id = int [@@deriving show, yojson]

type measure_id = int [@@deriving show, yojson]

type agent_kind = int [@@deriving show, yojson]

type site_name = int [@@deriving show, yojson]

type int_state = int [@@deriving show, yojson]

type site = agent_id * site_name [@@deriving show, yojson]

type agent_spec = {agent_kind: agent_kind} [@@deriving show, yojson]

type lnk_state =
  | Free
  | Bound_to of site
  | Bound_to_type of (agent_kind * site_name)
  | Bound_to_any
[@@deriving show, yojson]

type test =
  | Agent_exists of agent_id
  | Lnk_state_is of site * lnk_state
  | Int_state_is of site * int_state
[@@deriving show, yojson]

type modification =
  | Create of agent_id
  | Destroy of agent_id
  | Mod_int_state of site * int_state
  | Mod_lnk_state of site * lnk_state
[@@deriving show, yojson]

type pattern =
  { agents: agent_spec array
  ; tests: test list
  ; mods: modification list
  ; agent_constraints: agent_id Utils.int_map
        (* Maps trace-pattern agent ids to constrained local agents *) }
[@@deriving show, yojson]

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
  | Measure : (event_id * measure_id) -> 'a expr_body
  | Binop : 'a expr * ('a, 'b, 'c) binop * 'b expr -> 'c expr_body
  | Unop : ('a, 'b) unop * 'a expr -> 'b expr_body
  | Agent_id : agent_id -> int expr_body

(* Conversion to JSON *)

module ExprToJSON = struct
  (* let conv *)
end

(* Measures *)

type _ event_measure =
  | Time : float event_measure
  | Rule : string event_measure
  | Init_event : bool event_measure

type _ state_measure =
  | Int_state : (Agent.t * site_name) -> string state_measure
  | Count : pattern -> int state_measure
  | Component : agent_id -> agent_set state_measure
  | Nphos : agent_id -> int state_measure
  | Snapshot : string state_measure
  | Print_cc : agent_id -> string state_measure

type measure = {used_in_pattern: bool; measure_descr: measure_descr}

and measure_descr =
  | State_measure :
      state_measure_time * 'a expr_type * 'a state_measure
      -> measure_descr
  | Event_measure : 'a expr_type * 'a event_measure -> measure_descr

and state_measure_time = Before | After

(* Trace patterns *)

type event_pattern =
  { main_pattern: pattern
  ; with_clause: bool expr (* unsupported by the engine for now... *)
  ; rule_constraint: rule_constraint option }

and rule_constraint =
  | Init
  | End_of_trace (* Not supported yet *)
  | Rule of int list
  | Obs of string

type event =
  { event_id: event_id (* Index in query.pattern.events *)
  ; event_pattern: event_pattern option
  ; defining_rel: defining_relation option
  ; measures: measure array
  ; already_constrained_agents: agent_id list
  ; captured_agents: agent_id list
        (* Invariants:
           already_constrained_agents `union` captured_agents
           = keys of event_pattern.main_pattern.
           The defining_rel pattern only constrains agents of
           already_constrained_agents. *) }

and defining_relation =
  | First_after of event_id * event_pattern
  | Last_before of event_id * event_pattern

type ('a, 'b) tree = Tree of 'a * ('b * ('a, 'b) tree) list

(* A tree that tells in what order events should be discovered.
   The root of this tree corresponds to the root of the trace
   pattern. *)
type matching_tree = (event_id, defining_relation) tree

type trace_pattern =
  { agents: agent_kind array
  ; events: event array
  ; traversal_tree: matching_tree
        (* equality_constraints : (event_id * event_id) list ; *) }

type action = Print : 'a expr -> action | If : bool expr * action -> action

type query =
  { title: string option
  ; legend: string list option
  ; pattern: trace_pattern
  ; action: action
  ; every_clause: float option }
