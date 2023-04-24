(*****************************************************************************)
(* Simple Expression Language                                                *)
(*****************************************************************************)

open Aliases
open Value

(* Type for expressions *)

type t =
  | Unop of Query_ast.unop * t
  | Binop of t * Query_ast.binop * t
  | Concat of t * t
  | Count_agents of agent_kind list * t
  | Null_const
  | Int_const of int
  | Float_const of float
  | String_const of string
  | Measure of local_event_id * measure_id
  | Agent_id of local_agent_id
  | Event_id of local_event_id
[@@deriving show, yojson_of]

(* Typing utilities *)

let type_error op ~expected ~got =
  Error.fail
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
      Error.(
        fail
          (Type_error
             (Fmt.str "unable to test equality between values '%s' and '%s'."
                (Value.show v) (Value.show v') ) ) )

(* Special KaTie operations *)

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

let is_null = function VNull -> true | _ -> false

let eval_expr ~(read_measure : local_event_id -> measure_id -> Value.t)
    ~(read_agent_id : local_agent_id -> global_agent_id)
    ~(read_event_id : local_event_id -> global_event_id) =
  let rec eval = function
    | Agent_id local_id ->
        VInt (read_agent_id local_id)
    | Event_id local_id ->
        VInt (read_event_id local_id)
    | Measure (local_event_id, measure_id) ->
        read_measure local_event_id measure_id
    | Concat (e, e') ->
        let e = eval e in
        let e' = eval e' in
        concat e e'
    | Null_const ->
        VNull
    | Int_const c ->
        VInt c
    | Float_const x ->
        VFloat x
    | String_const s ->
        VString s
    | Unop (Not, e) ->
        let e = eval e in
        if is_null e then VNull else VBool (not (cast "not" TBool e))
    | Unop (Size, e) ->
        let e = eval e in
        if is_null e then VNull
        else VInt (AgentSet.size (cast "size" TAgentSet e))
    | Count_agents (ags, e) ->
        let e = eval e in
        if is_null e then VTuple (List.map (fun _ -> VNull) ags)
        else count_agents ags (cast "count" TAgentSet e)
    | Binop (e, op, e') -> (
        let e = eval e in
        let e' = eval e' in
        if (is_null e || is_null e') && op <> Eq then VNull
        else
          match op with
          | Add ->
              arith "+" ( + ) ( +. ) e e'
          | Sub ->
              arith "-" ( - ) ( -. ) e e'
          | Mul ->
              arith "*" ( * ) ( *. ) e e'
          | Gt ->
              comparison ">" ( > ) ( > ) e e'
          | Ge ->
              comparison ">=" ( >= ) ( >= ) e e'
          | Lt ->
              comparison "<" ( < ) ( < ) e e'
          | Le ->
              comparison "<=" ( <= ) ( <= ) e e'
          | Or ->
              bool_binop "||" ( || ) e e'
          | And ->
              bool_binop "&&" ( && ) e e'
          | Eq ->
              equal e e'
          | Similarity ->
              VFloat
                (set_similarity
                   (cast "similarity" TAgentSet e)
                   (cast "similarity" TAgentSet e') ) )
  in
  eval
