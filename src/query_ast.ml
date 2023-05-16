(*****************************************************************************)
(* Query AST                                                                 *)
(*****************************************************************************)

type identifier = string [@@deriving show, yojson]

type site =
  { site_name: string
  ; site_lnk_test: lnk_state option
  ; site_lnk_mod: lnk_state option
  ; site_int_test: string option
  ; site_int_mod: string option }
[@@deriving show, yojson]

and lnk_state =
  | Free
  | Bound of int
  | Bound_to_type of (string * string) (* agent kind, site name *)
  | Bound_to_any
[@@deriving show, yojson]

type agent =
  { ag_constr: identifier option
  ; ag_kind: string
  ; ag_sites: site list
  ; ag_mod: agent_mod option }
[@@deriving show, yojson]

and agent_mod = Create | Erase [@@deriving show, yojson]

type mixture_pat = agent list [@@deriving show, yojson]

type unop = Not | Size [@@deriving show, yojson]

type binop = Eq | Add | Mul | Sub | Gt | Ge | Lt | Le | Similarity | And | Or
[@@deriving show, yojson]

type event_expr = Ev of identifier [@@deriving show, yojson]

type state_expr = Before of event_expr | After of event_expr
[@@deriving show, yojson]

type state_measure =
  | Int_state of (identifier * identifier)
  | Component of identifier
  | Print_cc of identifier
  | Snapshot
[@@deriving show, yojson]

type event_measure = Time | Rule | Debug_event | Sim_event_id
[@@deriving show, yojson]

type expr =
  | Unop of unop * expr
  | Binop of expr * binop * expr
  | Count_agents of string * expr
  | Null_const
  | Int_const of int
  | Float_const of float
  | String_const of string
  | State_measure of state_expr * state_measure
  | Event_measure of event_expr * event_measure
  | Agent_id of identifier
  | Event_id of identifier
[@@deriving show, yojson]

type rule_constraint_disjunct = Rule of string | Init
[@@deriving show, yojson]

type rule_constraint = rule_constraint_disjunct list [@@deriving show, yojson]

type event_pattern =
  { event_id: identifier option
  ; main_pattern: mixture_pat
  ; rule_constraint: rule_constraint option }
[@@deriving show, yojson]

type trace_pattern = clause list [@@deriving show, yojson]

and clause =
  | Event of event_pattern
  | First_after of event_pattern * identifier
  | Last_before of event_pattern * identifier
[@@deriving show, yojson]

type action = Print of expr list [@@deriving show, yojson]

type t =
  { query_name: string
  ; legend: string list option
  ; pattern: trace_pattern
  ; when_clause: expr option
  ; every_clause: float option
  ; action: action }
[@@deriving show, yojson]
