(*****************************************************************************)
(* AST                                                                       *)
(*****************************************************************************)

type identifier = string

type site = {
  site_name : string ;
  site_lnk_test : lnk_state option ;
  site_lnk_mod  : lnk_state option ;
  site_int_test : string option ;
  site_int_mod  : string option ;
}

and lnk_state = 
  | Free 
  | Bound of int 
  | Bound_to_type of (string * string) (* agent kind, site name *)
  | Bound_to_any

type agent = {
  ag_constr : identifier option ;
  ag_kind : string ;
  ag_sites : site list ;
  ag_mod : agent_mod option ;
}

and agent_mod = Create | Erase

type mixture_pat = agent list

type unop = Not | Size

type binop = Eq | Add | Mul | Sub | Gt | Ge | Lt | Le

type event_expr = This | Ev of identifier

type state_expr = Before of event_expr | After of event_expr

type state_measure = 
  | Component of identifier
  | Nphos of identifier

type event_measure = 
  | Time
  | Rule

type expr = 
  | Unop of unop * expr
  | Binop of expr * binop * expr
  | Concat of expr * expr
  | Count_agents of string list * expr
  | Int_const of int
  | Float_const of float
  | String_const of string
  | State_measure of state_expr * state_measure
  | Event_measure of event_expr * event_measure

type rule_constraint = Rule of string list | Obs of string

type event_pattern = { 
  event_id : identifier option ;
  with_clause : expr option ;
  main_pattern : mixture_pat ;
  rule_constraint : rule_constraint option ;
}

type trace_pattern = clause list

and clause = 
  | Event of event_pattern
  | First_after of event_pattern * identifier
  | Last_before of event_pattern * identifier

type action = 
  | Print of expr

type query = {
  pattern : trace_pattern ;
  action : action ;
  legend : (string list) option ;
  query_name : string option ;
}
