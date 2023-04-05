(*****************************************************************************)
(* Simple Expression Language                                                      *)
(*****************************************************************************)

let f () = ()

open Aliases
open Value

(* Type for expressions *)

type t =
  | Unop of Query_ast.unop * t
  | Binop of t * Query_ast.binop * t
  | Concat of t * t
  | Count_agents of agent_kind list * t
  | Int_const of int
  | Float_const of float
  | String_const of string
  | Measure of local_event_id * measure_id
  | Agent_id of local_agent_id
[@@deriving show, yojson_of]

(* Typing utilities *)

let type_error op ~expected ~got =
  Tql_error.fail
    (Type_error
       (Fmt.str "in an argument to '%s', expected '%s' and got '%s'." op
          expected got ) )

let cast : type a. string -> a value_type -> Value.t -> a =
 fun op ty v ->
  match Value.cast ty v with
  | None ->
      type_error op
        ~expected:(value_type_to_string (T ty))
        ~got:(value_type_to_string (typeof v))
  | Some x ->
      x

(* Standard numerical and logical operations *)

let num_binop op_name int_op float_op e e' =
  match (e, e') with
  | VInt v, VInt v' ->
      int_op v v'
  | VFloat v, VFloat v' ->
      float_op v v'
  | VInt v, VFloat v' ->
      float_op (float_of_int v) v'
  | VFloat v, VInt v' ->
      float_op v (float_of_int v')
  | (VInt _ | VFloat _), v | v, _ ->
      type_error op_name ~expected:"int|float"
        ~got:(value_type_to_string (typeof v))

let arith op_name int_op float_op =
  num_binop op_name
    (fun x y -> VInt (int_op x y))
    (fun x y -> VFloat (float_op x y))

let comparison op_name int_op float_op =
  num_binop op_name
    (fun x y -> VBool (int_op x y))
    (fun x y -> VBool (float_op x y))

let bool_binop op_name op v v' =
  let v = cast op_name TBool v in
  let v' = cast op_name TBool v' in
  VBool (op v v')

let equal v v' =
  match Value.equal v v' with
  | Some b ->
      VBool b
  | None ->
      Tql_error.(
        fail
          (Type_error
             (Fmt.str "unable to test equality between values '%s' and '%s'."
                (Value.show v) (Value.show v') ) ) )

(* Special TQL operations *)

let concat v v' =
  let elts = function VTuple vs -> vs | v -> [v] in
  let tup = function [] -> VTuple [] | [x] -> x | vs -> VTuple vs in
  tup (elts v @ elts v')

let count_agents ags cc =
  let has_type k (_, k') = k = k' in
  ags
  |> List.map (fun ag -> AgentSet.size (AgentSet.filter (has_type ag) cc))
  |> List.map (fun i -> VInt i)
  |> fun l -> VTuple l

let set_similarity s s' =
  let open Utils in
  let ids_set ags = IntSet.of_list (List.map fst (AgentSet.elements ags)) in
  let s = ids_set s in
  let s' = ids_set s' in
  let denom = IntSet.cardinal (IntSet.union s s') in
  if denom = 0 then 1.0
  else
    let num = IntSet.cardinal (IntSet.inter s s') in
    float_of_int num /. float_of_int denom

(* Expression interpreter *)

type measurer = local_event_id -> measure_id -> Value.t

type agent_matching = local_agent_id -> global_agent_id

let eval_expr (read_measure : measurer) (matching : agent_matching) =
  let rec eval = function
    | Agent_id local_id ->
        VInt (matching local_id)
    | Measure (local_event_id, measure_id) ->
        read_measure local_event_id measure_id
    | Concat (e, e') ->
        concat (eval e) (eval e')
    | Int_const c ->
        VInt c
    | Float_const x ->
        VFloat x
    | String_const s ->
        VString s
    | Unop (Not, e) ->
        VBool (not (cast "not" TBool (eval e)))
    | Unop (Size, e) ->
        VInt (AgentSet.size (cast "size" TAgentSet (eval e)))
    | Binop (e, Add, e') ->
        arith "+" ( + ) ( +. ) (eval e) (eval e')
    | Binop (e, Sub, e') ->
        arith "-" ( - ) ( -. ) (eval e) (eval e')
    | Binop (e, Mul, e') ->
        arith "*" ( * ) ( *. ) (eval e) (eval e')
    | Binop (e, Gt, e') ->
        comparison ">" ( > ) ( > ) (eval e) (eval e')
    | Binop (e, Ge, e') ->
        comparison ">=" ( >= ) ( >= ) (eval e) (eval e')
    | Binop (e, Lt, e') ->
        comparison "<" ( < ) ( < ) (eval e) (eval e')
    | Binop (e, Le, e') ->
        comparison "<=" ( <= ) ( <= ) (eval e) (eval e')
    | Binop (e, Or, e') ->
        bool_binop "||" ( || ) (eval e) (eval e')
    | Binop (e, And, e') ->
        bool_binop "&&" ( && ) (eval e) (eval e')
    | Binop (e, Eq, e') ->
        equal (eval e) (eval e')
    | Binop (e, Similarity, e') ->
        VFloat
          (set_similarity
             (cast "similarity" TAgentSet (eval e))
             (cast "similarity" TAgentSet (eval e')) )
    | Count_agents (ags, e) ->
        count_agents ags (cast "count" TAgentSet (eval e))
  in
  eval
