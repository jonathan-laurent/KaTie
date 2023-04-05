(*****************************************************************************)
(*  Pretty printers for debugging purposes                                   *)
(*****************************************************************************)

open Format

let line =
  "-------------------------------------------------------------------------------"

let dline =
  "==============================================================================="

let dotline =
  "..............................................................................."

let pp_line f = fprintf f "%s@;" line

let pp_dline f = fprintf f "%s@;" dline

let pp_dotline f = fprintf f "%s@;" dotline

module type PRINTABLE = sig
  type t

  val pp : formatter -> t -> unit
end

let pp_list_inline pp_elem fmt l =
  let rec aux = function
    | [] ->
        ()
    | [x] ->
        fprintf fmt "%a" pp_elem x
    | x :: xs ->
        fprintf fmt "%a; " pp_elem x ;
        aux xs
  in
  fprintf fmt "[" ; aux l ; fprintf fmt "]"

let pp_list pp_elem fmt l = List.iter (fun x -> fprintf fmt "%a@;" pp_elem x) l

let pp_array pp_elem fmt tab =
  Array.iteri (fun i x -> fprintf fmt "%d: %a@;" i pp_elem x) tab

module PrintMap
    (E : Map.OrderedType)
    (M : Map.S with type key = E.t)
    (Print : PRINTABLE with type t = E.t) =
struct
  let pp_map (type a) (pp_elem : formatter -> a -> unit) (fmt : formatter)
      (map : a M.t) =
    map |> M.iter (fun k v -> fprintf fmt "%a: %a@;" Print.pp k pp_elem v)
end

module PrintInt = struct
  type t = int

  let pp fmt i = fprintf fmt "%d" i
end

let pp_int = PrintInt.pp

module PrintIntMap = PrintMap (Int) (Utils.IntMap) (PrintInt)

let pp_int_map = PrintIntMap.pp_map

open Matchings

module PrintAgVal = struct
  type t = Ag_valuation.t

  let pp = pp_list_inline PrintInt.pp
end

module PrintAgValMap = PrintMap (Ag_valuation) (ValMap) (PrintAgVal)

let pp_ag_val_map = PrintAgValMap.pp_map

let pp_option pp_elem fmt = function None -> () | Some x -> pp_elem fmt x

(*****************************************************************************)
(* Queries                                                                   *)
(*****************************************************************************)

let global_model : Model.t option ref = ref None

open Query

let box f msg = pp_line f ; fprintf f "| %s@;" msg ; pp_line f

let dbox f msg = pp_dline f ; fprintf f "| %s@;" msg ; pp_dline f

let dotbox f msg = pp_dotline f ; fprintf f "| %s@;" msg ; pp_dotline f

let pp_newline f = fprintf f "@;"

let rec expr_measures : type a. a expr -> (int * int) list =
 fun (body, _ty) ->
  match body with
  | Const _ ->
      []
  | Unop (_, e) ->
      expr_measures e
  | Binop (lhs, _, rhs) ->
      expr_measures lhs @ expr_measures rhs
  | Measure m ->
      [m]
  | Agent_id _ ->
      []

let pp_int2 f (x, y) = fprintf f "(%d, %d)" x y

let pp_expr f e =
  let mes = expr_measures e in
  fprintf f "expr involving measures %a" (pp_list_inline pp_int2) mes

let rec pp_action f = function
  | Print e ->
      pp_expr f e ; pp_newline f
  | If (cond, act) ->
      fprintf f "Test: %a@.Expr: %a@." pp_expr cond pp_action act

let pp_rule f r =
  match !global_model with
  | None ->
      Model.print_rule ~noCounters:false f r
  | Some env ->
      Model.print_rule ~noCounters:false ~env f r

let pp_agent_kind f i =
  match !global_model with
  | None ->
      fprintf f "%d" i
  | Some env ->
      fprintf f "%a(%d)" (Signature.print_agent (Model.signatures env)) i i

let pp_rule_constraint f = function
  | Init ->
      fprintf f "init"
  | End_of_trace ->
      fprintf f "eot"
  | Obs s ->
      fprintf f "obs(%s)" s
  | Rule rs ->
      fprintf f "rules[%d](%a)" (List.length rs) (pp_list_inline pp_rule) rs

let pp_agent_descr f {agent_kind} = pp_agent_kind f agent_kind

let pp_site f (ag_id, s) =
  let site_name =
    match !global_model with
    | None ->
        asprintf "%d" s
    | Some _env ->
        asprintf "%d" s
  in
  fprintf f "(%d, %s)" ag_id site_name

let pp_int_state = pp_int

let pp_lnk_state f = function
  | Free ->
      fprintf f "free"
  | Bound_to s ->
      fprintf f "bound%a" pp_site s
  | Bound_to_any ->
      fprintf f "bound"
  | Bound_to_type _ ->
      fprintf f "bound-to-type"

let pp_test f = function
  | Agent_exists aid ->
      fprintf f "exists(%d)" aid
  | Lnk_state_is (s, st) ->
      fprintf f "lnk%a = %a" pp_site s pp_lnk_state st
  | Int_state_is (s, st) ->
      fprintf f "int%a = %a" pp_site s pp_int_state st

let pp_mod f = function
  | Create aid ->
      fprintf f "add(%d)" aid
  | Destroy aid ->
      fprintf f "rm(%d)" aid
  | Mod_int_state (s, st) ->
      fprintf f "int%a := %a" pp_site s pp_int_state st
  | Mod_lnk_state (s, st) ->
      fprintf f "lnk%a := %a" pp_site s pp_lnk_state st

let pp_measure_descr f = function
  | Event_measure (_, Time) ->
      fprintf f "time"
  | Event_measure (_, Rule) ->
      fprintf f "rule"
  | State_measure (_, _, Component ag) ->
      fprintf f "component(%d)" ag
  | State_measure (_, _, Nphos ag) ->
      fprintf f "nphos(%d)" ag
  | State_measure (_, _, Int_state ((ag_id, ag_kind), s)) ->
      fprintf f "int_state((%d, %d), %d)" ag_id ag_kind s
  | _ ->
      fprintf f "<measure>"

let pp_measure f {measure_descr; _} = pp_measure_descr f measure_descr

let pp_main_pattern f (p : pattern) =
  fprintf f "agents:@;%a" (pp_array pp_agent_descr) p.agents ;
  pp_newline f ;
  fprintf f "ag-constraints:@;%a" (pp_int_map pp_int) p.agent_constraints ;
  pp_newline f ;
  fprintf f "tests:@;%a" (pp_list pp_test) p.tests ;
  pp_newline f ;
  fprintf f "mods:@;%a" (pp_list pp_mod) p.mods

let pp_event_pattern f {main_pattern; with_clause; rule_constraint} =
  fprintf f "with-clause: %a@;" pp_expr with_clause ;
  ( match rule_constraint with
  | None ->
      ()
  | Some rule_constraint ->
      fprintf f "rule-constraint: %a@;" pp_rule_constraint rule_constraint ) ;
  pp_newline f ;
  pp_main_pattern f main_pattern

let pp_event f ev =
  box f (sprintf "[%d]" ev.event_id) ;
  fprintf f "captured-agents: %a@;" (pp_list_inline pp_int) ev.captured_agents ;
  fprintf f "already-constrained: %a@;" (pp_list_inline pp_int)
    ev.already_constrained_agents ;
  fprintf f "measures: %a@;"
    (pp_list_inline pp_measure)
    (Array.to_list ev.measures) ;
  pp_dotline f ;
  ( match ev.event_pattern with
  | None ->
      ()
  | Some ep ->
      dotbox f (asprintf "Event pattern") ;
      pp_event_pattern f ep ) ;
  let print_def_rel msg e ep =
    dotbox f (asprintf "%s [%d]" msg e) ;
    pp_event_pattern f ep
  in
  ( match ev.defining_rel with
  | Some (First_after (e, ep)) ->
      print_def_rel "First after" e ep
  | Some (Last_before (e, ep)) ->
      print_def_rel "Last before" e ep
  | None ->
      () ) ;
  pp_newline f

let rec pp_tree f (Tree (ag_id, children)) =
  let pp_child f (_rel, t) = pp_tree f t in
  fprintf f "{%d, %a}" ag_id (pp_list_inline pp_child) children

let pp_trace_pattern f tp =
  fprintf f "Agents@;" ;
  pp_line f ;
  pp_array pp_agent_kind f tp.agents ;
  pp_line f ;
  fprintf f "Events@;" ;
  pp_dline f ;
  Array.iter (pp_event f) tp.events ;
  pp_dline f ;
  fprintf f "Tree@;" ;
  pp_tree f tp.traversal_tree ;
  pp_newline f

let pp_query f {pattern; action; _} =
  fprintf f "@[<v>" ;
  pp_dline f ;
  fprintf f "| QUERY DESCRIPTION@;" ;
  dbox f "Pattern" ;
  pp_trace_pattern f pattern ;
  dbox f "Action" ;
  pp_action f action ;
  pp_dline f ;
  fprintf f "@]"
