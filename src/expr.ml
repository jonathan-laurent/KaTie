(*****************************************************************************)
(* Simple Expression Language                                                      *)
(*****************************************************************************)

let f () = ()

open Aliases
open Value

type unop = Query_ast.unop

type binop = Query_ast.binop

type t =
  | Unop of unop * t
  | Binop of t * binop * t
  | Concat of t * t
  | Count_agents of agent_kind list * t
  | Int_const of int
  | Float_const of float
  | String_const of string
  | Measure of local_event_id * measure_id
  | Agent_id of local_agent_id

type measurer = local_event_id * measure_id -> Value.t option

type agent_matching = local_agent_id -> global_agent_id

let bool_to_int = function true -> 1 | false -> 0

let count_agents ags cc =
  let has_type k (_, k') = k = k' in
  ags
  |> List.map (fun ag -> AgentSet.size (AgentSet.filter (has_type ag) cc))
  |> List.map (fun i -> Int i)
  |> fun l -> Tuple l

let cast : type a. string -> a value_type -> Value.t -> a =
 fun op ty v ->
  match Value.cast ty v with
  | Either.Left {expected; got} ->
      Tql_error.fail
        (Type_error
           (Fmt.str "in an argument to '%s', expected '%s' and got '%s'." op
              expected got ) )
  | Either.Right x ->
      x

(* let eval_expr (read_measure : measurer) (read_id : agent_matching) =
   let open Query_ast in
   let rec eval = function
     | Unop (Not, e) ->
         Bool (not (cast "not" Ty_Bool (eval e)))
     | Unop (Size, e) ->
         Int (AgentSet.size (cast "size" Ty_Agent_set (eval e)))
   in
   eval *)
