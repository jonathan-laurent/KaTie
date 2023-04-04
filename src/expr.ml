(*****************************************************************************)
(* Simple Expression Language                                                      *)
(*****************************************************************************)

let f () = print_endline "Hello !"

open Aliases

type unop = Query_ast.unop

type binop = Query_ast.binop

type t =
  | Unop of unop * t
  | Binop of t * binop * t
  | Concat of t * t
  | Count_agents of string list * t
  | Int_const of int
  | Float_const of float
  | String_const of string
  | Measure of local_event_id * measure_id
  | Agent_id of local_agent_id
