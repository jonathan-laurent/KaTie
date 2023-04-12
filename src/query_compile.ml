(*****************************************************************************)
(* Compilation of queries                                                    *)
(*****************************************************************************)

(* The following is done during compilation:
   + Agent kinds and site names are translated from strings to integer
     using the model.
   + Mixture patterns are reformulated to make tests, modifications
     and constraints more easily accessible.
   + Expressions are typechecked
   + Measures are gathered for each event
   + A traversal tree is computed
*)

module Ast = Query_ast
open Query
module IntMap = Utils.IntMap
module SMap = Utils.StringMap

(*****************************************************************************)
(* Useful data structures                                                    *)
(*****************************************************************************)

module PreArray = struct
  (* A queue-like structure to build arrays, where the final ID of each
     agent is known from the moment it is added into the queue. *)
  type 'a t = {elems: 'a Queue.t; mutable next_fresh_id: int}

  let create () = {elems= Queue.create (); next_fresh_id= 0}

  let fresh_id t =
    let id = t.next_fresh_id in
    t.next_fresh_id <- id + 1 ;
    id

  let add t e =
    let id = fresh_id t in
    Queue.push e t.elems ; id

  let to_array t = Utils.array_of_queue t.elems
end

module Dict = struct
  (* A datastructure to maintain a collection of mutable elements
     indexed by both numerical ids and (optionally) string ids. When an
     element is accessed using a numerical id, it is created and
     initialized if it does not exist already. Numerical ids can be
     obtained from string names (using [id_of_name]) or by creating new
     anonymous elements explicitly (using [new_anonymous]). *)
  (* Invariant: For every i in [0,next_fresh_id) and no other, `elems`
     maps i to an element. For the invariant to hold, ids need to be
     obtained using `id_of_name` and `new_anonymous`. *)
  type 'a t =
    { name_to_id: (string, int) Hashtbl.t
    ; mutable next_fresh_id: int
    ; mutable elems: 'a IntMap.t
    ; create_elem: unit -> 'a }

  let create create_elem =
    { name_to_id= Hashtbl.create 100
    ; next_fresh_id= 0
    ; elems= IntMap.empty
    ; create_elem }

  let fresh_id t =
    let id = t.next_fresh_id in
    t.next_fresh_id <- id + 1 ;
    id

  (* Creates an anonymous element and returns its numerical id. *)
  let new_anonymous t =
    let id = fresh_id t in
    let elem = t.create_elem () in
    t.elems <- IntMap.add id elem t.elems ;
    id

  (* Get a mutable element from the table, creating it if it does not
     exist already. *)
  let get t id =
    try IntMap.find id t.elems
    with Not_found ->
      let elem = t.create_elem () in
      t.elems <- IntMap.add id elem t.elems ;
      elem

  (* Access the numerical id associated to a name and create a fresh id
     with this name if necessary. *)
  let id_of_name t name =
    try Hashtbl.find t.name_to_id name
    with Not_found ->
      let id = fresh_id t in
      Hashtbl.add t.name_to_id name id ;
      ignore (get t id) ;
      id

  let to_array t =
    let n = t.next_fresh_id in
    try Array.init n (fun i -> IntMap.find i t.elems)
    with Not_found -> assert false
end

(*****************************************************************************)
(* Parse whole query to find constrained agent's types                       *)
(*****************************************************************************)

(* This will go away *)
let clause_pattern = function
  | Ast.Event e ->
      e
  | Ast.First_after (e, _) ->
      e
  | Ast.Last_before (e, _) ->
      e

(* This is needed because Int_state_measure used to store an agent_kind
   but won't be needed anymore. *)
let constrained_agents_types q =
  q.Ast.pattern
  |> List.fold_left
       (fun acc clause ->
         let pat = clause_pattern clause in
         pat.Ast.main_pattern
         |> List.fold_left
              (fun acc ag ->
                match ag.Ast.ag_constr with
                | None ->
                    acc
                | Some id ->
                    let kind = ag.Ast.ag_kind in
                    SMap.add id kind acc )
              acc )
       SMap.empty

(*****************************************************************************)
(* Compilation environment                                                   *)
(*****************************************************************************)

open Aliases

type tmp_event =
  { tmp_ev_measures: measure_descr PreArray.t
  ; tmp_main_pats: event_pattern Queue.t
  ; tmp_def_rels: defining_relation Queue.t }

type tmp_agent = {mutable tmp_ag_kind: agent_kind option}

type env =
  { model: Model.t
  ; query_agents: tmp_agent Dict.t
  ; query_events: tmp_event Dict.t
  ; constrained_agents_types: string SMap.t (* TODO: remove*) }

let create_env (model : Model.t) (q : Ast.query) =
  let create_agent () = {tmp_ag_kind= None} in
  let create_event () =
    { tmp_ev_measures= PreArray.create ()
    ; tmp_main_pats= Queue.create ()
    ; tmp_def_rels= Queue.create () }
  in
  { model
  ; query_agents= Dict.create create_agent
  ; query_events= Dict.create create_event
  ; constrained_agents_types= constrained_agents_types q }

(*****************************************************************************)
(* Compile measures                                                          *)
(*****************************************************************************)

let eval_ev_expr env cur_ev_id = function
  | Ast.Ev name ->
      Dict.id_of_name env.query_events name
  | Ast.This -> (
    match cur_ev_id with
    | None ->
        Log.failwith "Unexpected usage of Ast.This."
    | Some id ->
        id )

let eval_st_expr env cur_ev_id = function
  | Ast.Before ev_expr ->
      (eval_ev_expr env cur_ev_id ev_expr, Measure.Before)
  | Ast.After ev_expr ->
      (eval_ev_expr env cur_ev_id ev_expr, Measure.After)

let register_measure in_action _cur_ev_id ev_id ev measure =
  let used_in_pattern = not in_action in
  let m_id = PreArray.add ev.tmp_ev_measures {used_in_pattern; measure} in
  Expr.Measure (ev_id, m_id)

let compile_event_measure env in_action cur_ev_id ev_expr m =
  let ev_id = eval_ev_expr env cur_ev_id ev_expr in
  let ev = Dict.get env.query_events ev_id in
  let measure =
    match m with
    | Ast.Time ->
        Measure.(Event_measure Time)
    | Ast.Rule ->
        Measure.(Event_measure Rule)
    | Ast.Init_event ->
        Measure.(Event_measure Init_event)
    | Ast.Debug_event ->
        Measure.(Event_measure Debug_event)
  in
  register_measure in_action cur_ev_id ev_id ev measure

let tr_agent env ag_name = Dict.id_of_name env.query_agents ag_name

let tr_agent_kind env s =
  Signature.num_of_agent (Locality.dummy_annot s) (Model.signatures env.model)

let tr_site_name env ag_kind_id s =
  Signature.num_of_site (Locality.dummy_annot s)
    (Signature.get (Model.signatures env.model) ag_kind_id)

let tr_int_state env ag_id s_id st =
  Signature.num_of_internal_state s_id (Locality.dummy_annot st)
    (Signature.get (Model.signatures env.model) ag_id)

(* A quark is something like k.x int int_state[]{k.x} *)
let tr_quark env (ag_name, site_name) =
  let ag_id = tr_agent env ag_name in
  let ag_kind_name =
    try SMap.find ag_name env.constrained_agents_types
    with Not_found -> Log.failwith "Illegal agent usage."
  in
  let agent_kind = tr_agent_kind env ag_kind_name in
  let site_id = tr_site_name env agent_kind site_name in
  (ag_id, site_id)

let compile_state_measure env in_action cur_ev_id st_expr m =
  let ev_id, m_time = eval_st_expr env cur_ev_id st_expr in
  let ev = Dict.get env.query_events ev_id in
  let measure =
    match m with
    | Ast.Nphos _ ->
        Tql_error.failwith "The 'nphos' measure is unimplemented."
    | Ast.Component ag_name ->
        Measure.(State_measure (m_time, Component (tr_agent env ag_name)))
    | Ast.Int_state quark ->
        Measure.(State_measure (m_time, Int_state (tr_quark env quark)))
    | Ast.Snapshot ->
        Measure.(State_measure (m_time, Snapshot))
    | Ast.Print_cc ag_name ->
        Measure.(State_measure (m_time, Print_cc (tr_agent env ag_name)))
  in
  register_measure in_action cur_ev_id ev_id ev measure

(*****************************************************************************)
(* Compile expressions                                                       *)
(*****************************************************************************)

let rec compile_expr env in_action cur_ev_id e =
  match e with
  | Ast.Int_const i ->
      Expr.Int_const i
  | Ast.Float_const f ->
      Expr.Float_const f
  | Ast.String_const s ->
      Expr.String_const s
  | Ast.Unop (op, arg) ->
      Expr.Unop (op, compile_expr env in_action cur_ev_id arg)
  | Ast.Binop (lhs, op, rhs) ->
      let lhs = compile_expr env in_action cur_ev_id lhs in
      let rhs = compile_expr env in_action cur_ev_id rhs in
      Expr.Binop (lhs, op, rhs)
  | Ast.State_measure (st, m) ->
      compile_state_measure env in_action cur_ev_id st m
  | Ast.Event_measure (ev, m) ->
      compile_event_measure env in_action cur_ev_id ev m
  | Ast.Concat (lhs, rhs) ->
      let lhs = compile_expr env in_action cur_ev_id lhs in
      let rhs = compile_expr env in_action cur_ev_id rhs in
      Expr.Concat (lhs, rhs)
  | Ast.Count_agents (ag_kinds, arg) ->
      let arg = compile_expr env in_action cur_ev_id arg in
      let ags = List.map (tr_agent_kind env) ag_kinds in
      Expr.Count_agents (ags, arg)
  | Ast.Agent_id ag_name ->
      Expr.Agent_id (tr_agent env ag_name)

(*****************************************************************************)
(* Compile mixture patterns                                                  *)
(*****************************************************************************)

let compile_mixture_pattern env ags =
  let ags_array = Array.mapi (fun i x -> (i, x)) (Array.of_list ags) in
  let agents =
    ags_array
    |> Array.map (fun (_, ag) -> {agent_kind= tr_agent_kind env ag.Ast.ag_kind})
  in
  let fold_sites f acc =
    ags_array
    |> Array.fold_left
         (fun acc (ag_id, ag) ->
           ag.Ast.ag_sites
           |> List.fold_left
                (fun acc s ->
                  let ag_kind_id = agents.(ag_id).agent_kind in
                  let site_id = tr_site_name env ag_kind_id s.Ast.site_name in
                  f acc ag_id ag_kind_id ag site_id s )
                acc )
         acc
  in
  (* bond_id -> (pat_agent_id, site_id) *)
  let bonds : (int, int * int) Hashtbl.t = Hashtbl.create 10 in
  let tests =
    fold_sites
      (fun acc ag_id ag_kind_id _ag site_id ast_site ->
        let site = (ag_id, site_id) in
        let acc =
          match ast_site.Ast.site_int_test with
          | None ->
              acc
          | Some st ->
              let st = tr_int_state env ag_kind_id site_id st in
              Int_state_is (site, st) :: acc
        in
        let acc =
          match ast_site.Ast.site_lnk_test with
          | None ->
              acc
          | Some Ast.Free ->
              Lnk_state_is (site, Free) :: acc
          | Some (Ast.Bound b) -> (
            try
              let dest = Hashtbl.find bonds b in
              Lnk_state_is (site, Bound_to dest)
              :: Lnk_state_is (dest, Bound_to site)
              :: acc
            with Not_found -> Hashtbl.add bonds b site ; acc )
          | Some (Ast.Bound_to_type (ag_kind, site_name)) ->
              let ag_kind = tr_agent_kind env ag_kind in
              let site_name = tr_site_name env ag_kind site_name in
              Lnk_state_is (site, Bound_to_type (ag_kind, site_name)) :: acc
          | Some Ast.Bound_to_any ->
              Lnk_state_is (site, Bound_to_any) :: acc
        in
        acc )
      []
  in
  Hashtbl.reset bonds ;
  let mods =
    fold_sites
      (fun acc ag_id ag_kind_id _ag site_id ast_site ->
        let site = (ag_id, site_id) in
        let acc =
          match ast_site.Ast.site_int_mod with
          | None ->
              acc
          | Some st ->
              let st = tr_int_state env ag_kind_id site_id st in
              Mod_int_state (site, st) :: acc
        in
        let acc =
          match ast_site.Ast.site_lnk_mod with
          | None ->
              acc
          | Some Ast.Free ->
              Mod_lnk_state (site, Free) :: acc
          | Some (Ast.Bound b) -> (
            try
              let dest = Hashtbl.find bonds b in
              Mod_lnk_state (site, Bound_to dest)
              :: Mod_lnk_state (dest, Bound_to site)
              :: acc
            with Not_found -> Hashtbl.add bonds b site ; acc )
          | Some (Ast.Bound_to_type (ag_kind, site_name)) ->
              let ag_kind = tr_agent_kind env ag_kind in
              let site_name = tr_site_name env ag_kind site_name in
              Mod_lnk_state (site, Bound_to_type (ag_kind, site_name)) :: acc
          | Some Ast.Bound_to_any ->
              Mod_lnk_state (site, Bound_to_any) :: acc
        in
        acc )
      []
  in
  let mods =
    ags_array
    |> Array.fold_left
         (fun acc (ag_id, ag) ->
           match ag.Ast.ag_mod with
           | None ->
               acc
           | Some Ast.Create ->
               Create ag_id :: acc
           | Some Ast.Erase ->
               Destroy ag_id :: acc )
         mods
  in
  let agent_constraints =
    ags_array
    |> Array.fold_left
         (fun acc (pat_ag_id, ag) ->
           match ag.Ast.ag_constr with
           | None ->
               acc
           | Some name ->
               let qid = Dict.id_of_name env.query_agents name in
               (Dict.get env.query_agents qid).tmp_ag_kind <-
                 Some agents.(pat_ag_id).agent_kind ;
               IntMap.add qid pat_ag_id acc )
         IntMap.empty
  in
  {agents; tests; mods; agent_constraints}

(*****************************************************************************)
(* Compile events                                                            *)
(*****************************************************************************)

let compile_rule_constraint env = function
  | Some (Ast.Rule rs) ->
      Some (Rule (List.concat_map (fun r -> Model.nums_of_rule r env.model) rs))
  | Some (Ast.Obs s) ->
      Some (Obs s)
  | None ->
      None

let compile_with_clause env name_opt = function
  | None ->
      None
  | Some wc ->
      Some (compile_expr env false name_opt wc)

let compile_event_pattern env pat =
  let cur_ev_id =
    match pat.Ast.event_id with
    | None ->
        Dict.new_anonymous env.query_events
    | Some name ->
        Dict.id_of_name env.query_events name
  in
  let with_clause =
    compile_with_clause env (Some cur_ev_id) pat.Ast.with_clause
  in
  let rule_constraint = compile_rule_constraint env pat.Ast.rule_constraint in
  let main_pattern = compile_mixture_pattern env pat.Ast.main_pattern in
  (cur_ev_id, {main_pattern; with_clause; rule_constraint})

(* We compile a trace pattern by going over each clause sequentially. *)
let process_clauses env (tpat : Ast.clause list) =
  tpat
  |> List.iter (function
       | Ast.Event ev_pat ->
           let ev_id, main_pat = compile_event_pattern env ev_pat in
           let ev = Dict.get env.query_events ev_id in
           Queue.push main_pat ev.tmp_main_pats
       | Ast.First_after (ev_pat, ref_name) ->
           let ref_id = Dict.id_of_name env.query_events ref_name in
           let ev_id, pat = compile_event_pattern env ev_pat in
           let ev = Dict.get env.query_events ev_id in
           let rel = First_after (ref_id, pat) in
           Queue.push rel ev.tmp_def_rels
       | Ast.Last_before (ev_pat, ref_name) ->
           let ref_id = Dict.id_of_name env.query_events ref_name in
           let ev_id, pat = compile_event_pattern env ev_pat in
           let ev = Dict.get env.query_events ev_id in
           let rel = Last_before (ref_id, pat) in
           Queue.push rel ev.tmp_def_rels )

let make_event i tmp_ev =
  { event_id= i
  ; event_pattern=
      ( match Utils.list_of_queue tmp_ev.tmp_main_pats with
      | [] ->
          None
      | [p] ->
          Some p
      | _ ->
          Tql_error.(
            fail
              (Compilation_error
                 "There can be at most one main clause for every event." ) ) )
  ; defining_rel=
      ( match Utils.list_of_queue tmp_ev.tmp_def_rels with
      | [] ->
          None
      | [r] ->
          Some r
      | _ ->
          Tql_error.(
            fail
              (Compilation_error
                 "There can be at most one defining relation for an event." ) )
      )
  ; measures= PreArray.to_array tmp_ev.tmp_ev_measures
  ; already_constrained_agents= []
  ; captured_agents= [] }

let make_agent {tmp_ag_kind} =
  match tmp_ag_kind with
  | None ->
      Tql_error.(fail (Compilation_error "Unbound agent identifier."))
  | Some k ->
      k

let compile_trace_pattern env tpat =
  process_clauses env tpat ;
  let events = Array.mapi make_event (Dict.to_array env.query_events) in
  let agents = Array.map make_agent (Dict.to_array env.query_agents) in
  (* Still uncomplete: we put a dummy value *)
  let traversal_tree = Tree (-1, []) in
  {agents; events; traversal_tree}

(*****************************************************************************)
(* Compile action                                                            *)
(*****************************************************************************)

let compile_action env when_clause = function
  | Ast.Print e -> (
      let e = compile_expr env true None e in
      match when_clause with None -> Print e | Some b -> If (b, Print e) )

(*****************************************************************************)
(* Compile queries                                                           *)
(*****************************************************************************)

(* We first compute a dependency graph for the query. *)
(* This graph is a tree since nodes have at most one predecessor and
   there can only be one root. *)

(* A, last B before A, last C before B *)
(* A <- B <- C *)

(* For every clause "last e: {} before f", then f is a predecessor of e
   in the dependency graph (it must be resolved before e). *)
let predecessor ev =
  match ev.defining_rel with
  | None ->
      None
  | Some (First_after (pid, _) as rel) ->
      Some (rel, pid)
  | Some (Last_before (pid, _) as rel) ->
      Some (rel, pid)

let compute_traversal_tree (q : query) =
  let roots = Queue.create () in
  (* We compute the inverse of the precedence relation. *)
  (* Note that hash-tables in OCaml can map each key to several values. *)
  let succs = Hashtbl.create 100 in
  q.pattern.events
  |> Array.iteri (fun ev_id ev ->
         match predecessor ev with
         | None ->
             Queue.push ev_id roots
         | Some (rel, pred_id) ->
             Hashtbl.add succs pred_id (rel, ev_id) ) ;
  (* The roots are the nodes without predecessor. We only accept queries
     with a single root. *)
  let roots = Utils.list_of_queue roots in
  match roots with
  | [] ->
      Tql_error.(fail No_root_event)
  | _ :: _ :: _ ->
      Tql_error.(fail Disconnected_query_graph)
  | [root_id] ->
      let rec build_tree i =
        let children =
          Hashtbl.find_all succs i
          |> List.map (fun (rel, j) -> (rel, build_tree j))
        in
        Tree (i, children)
      in
      build_tree root_id

let def_rel_pattern ev =
  match ev.defining_rel with
  | None ->
      None
  | Some (First_after (_, p)) ->
      Some p
  | Some (Last_before (_, p)) ->
      Some p

let opt_pattern_constrained_agents = function
  | None ->
      []
  | Some p ->
      List.map fst (IntMap.bindings p.main_pattern.agent_constraints)

(* The agents constrained by an event are those constrained by its main
   pattern and dependency?? *)
let constrained_agents (ev : event) =
  Utils.IntSet.of_list
  @@ opt_pattern_constrained_agents ev.event_pattern
  @ opt_pattern_constrained_agents (def_rel_pattern ev)

let schedule_agents_capture q =
  let module IntSet = Utils.IntSet in
  let rec aux (Tree (i, children)) constrained =
    let ev = q.pattern.events.(i) in
    let ev_ags = constrained_agents ev in
    let acas, cas =
      IntSet.partition (fun j -> IntSet.mem j constrained) ev_ags
    in
    let already_constrained_agents = IntSet.elements acas in
    let captured_agents = IntSet.elements cas in
    q.pattern.events.(i) <- {ev with already_constrained_agents; captured_agents} ;
    let constrained = IntSet.union constrained ev_ags in
    children |> List.iter (fun (_r, t) -> aux t constrained)
  in
  aux q.pattern.traversal_tree IntSet.empty

let schedule_execution (q : query) =
  let traversal_tree = compute_traversal_tree q in
  let q = {q with pattern= {q.pattern with traversal_tree}} in
  schedule_agents_capture q ; q

let compile (model : Model.t) (q : Ast.query) =
  let title = q.Ast.query_name in
  Log.with_current_query title (fun () ->
      Log.set_current_query title ;
      let env = create_env model q in
      (* Compile the action first so that no measure is missing in the
         pattern *)
      let when_clause =
        Option.map (compile_expr env true None) q.Ast.when_clause
      in
      let action = compile_action env when_clause q.Ast.action in
      let pattern = compile_trace_pattern env q.Ast.pattern in
      let legend = q.Ast.legend in
      let every_clause = q.Ast.every_clause in
      schedule_execution {pattern; action; title; legend; every_clause} )
