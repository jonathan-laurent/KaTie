(*****************************************************************************)
(* Evaluate expressions                                                      *)
(*****************************************************************************)

open Query
open Utils
open Format

let expr_type : type a. a expr -> a expr_type = snd

let string_of_type : type a. a expr_type -> string = function
    | Bool -> "bool"
    | Int -> "int"
    | Float -> "float"
    | String -> "string"
    | Agent_set -> "agents"
    | Tuple -> "tuple"

let print_type fmt t = Format.fprintf fmt "%s" (string_of_type t)

let print_value_type fmt = function
    | Val (_x, t) -> print_type fmt t

let bool_to_int = function
    | true  -> 1
    | false -> 0

let cast_to (type a) (t : a expr_type) (v : value) : a option =
    match t, v with
    | Bool,      Val (x, Bool)      -> Some x
    | Int,       Val (x, Bool)      -> Some (bool_to_int x)
    | Int,       Val (x, Int)       -> Some x
    | Float,     Val (x, Float)     -> Some x
    | String,    Val (x, String)    -> Some x
    | Agent_set, Val (x, Agent_set) -> Some x
    | Tuple,     Val (x, Tuple)     -> Some x
    | Tuple,     Val (x, ty)        -> Some [Val (x, ty)]
    | _ ->
        Format.printf "Wrong cast attempt from `%a` to `%a`.\n"
            print_value_type v print_type t ;
        None

let rec compare_lists cmp xs ys =
    match xs, ys with
    | [], [] -> 0
    | [], _::_ -> -1
    | _::_, [] -> 1
    | x::xs, y::ys ->
        let c = cmp x y in
        if c = 0 then compare_lists cmp xs ys
        else c

let eval_compare : type a . a expr_type -> a -> a -> int =
    fun ty lhs rhs ->
    match ty with
        | Agent_set -> Agent.SetMap.Set.compare lhs rhs
        | Tuple -> failwith "Cannot compare tuples."
        | _ -> compare lhs rhs


let eval_eq ty lhs rhs = eval_compare ty lhs rhs = 0

let to_tuple : type a. a expr_type -> a -> tuple =
    fun ty x ->
    match ty with
    | Tuple -> x
    | _ -> [Val (x, ty)]

let eval_concat : type a b . a expr_type -> a -> b expr_type -> b -> tuple =
    fun ty_lhs lhs ty_rhs rhs ->
    to_tuple ty_lhs lhs @ to_tuple ty_rhs rhs

module AgSet = Agent.SetMap.Set

let count_agents ags cc =
    let has_type k (_, k') = k = k' in
    ags
    |> List.map (fun ag -> AgSet.size (AgSet.filter (has_type ag) cc))
    |> List.map (fun i -> Val (i, Int))


type measures_provider = ((event_id * measure_id) -> value option)
type agent_ids_provider = int -> int option

let rec eval_expr : type a. measures_provider -> agent_ids_provider -> a expr -> a option =
    fun read_measure read_id expr ->
    match expr with
    | (Const x, _) -> Some x
    | (Measure id, ty) -> bind_option (read_measure id) (cast_to ty)
    | (Binop (lhs_e, op, rhs_e), ty) ->
        begin match eval_expr read_measure read_id lhs_e, op, eval_expr read_measure read_id rhs_e with
        | Some lhs, Binop op, Some rhs -> Some (op lhs rhs)
        | Some lhs, Eq, Some rhs -> Some (eval_eq (expr_type lhs_e) lhs rhs)
        | Some lhs, Concat, Some rhs -> Some (eval_concat (expr_type lhs_e) lhs (expr_type rhs_e) rhs)
        | None, _, _ | _, _, None -> Printf.printf "Failed to eval expr.\n" ; None
        end
    | (Unop (Unop op, arg), ty) -> map_option op (eval_expr read_measure read_id arg)
    | (Unop (Count_agents ags, arg), ty) ->
        let cc = eval_expr read_measure read_id arg in
        cc |> Utils.map_option (count_agents ags)
    | (Agent_id qid, _) -> read_id qid

let eval_expr_to_value : type a. measures_provider -> agent_ids_provider -> a expr -> value option =
    fun read_measure read_id expr ->
    map_option (fun v -> Val (v, expr_type expr)) (eval_expr read_measure read_id expr)

let rec print_list print_el fmt = function
    | [] -> ()
    | [x] -> print_el fmt x
    | x::xs -> fprintf fmt "%a, %a" print_el x (print_list print_el) xs

let rec print_value fmt = function
    | Val (x, Bool) -> fprintf fmt "%d" (Utils.int_of_bool x)
    | Val (x, Int) -> fprintf fmt "%d" x
    | Val (x, Float) -> fprintf fmt "%.17g" x
    | Val (x, String) -> fprintf fmt "'%s'" x
    | Val (_x, Agent_set) -> fprintf fmt "<agents>"
    | Val (x, Tuple) -> print_list print_value fmt x

