(*****************************************************************************)
(* Compilation of queries                                                    *)
(*****************************************************************************)

(*  The following is done during compilation:
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

module PreArray = 
struct
    type 'a t = {
        elems : 'a Queue.t ;
        mutable next_fresh_id : int ;
    }

    let create () = {
        elems = Queue.create () ;
        next_fresh_id = 0 ;
    }

    let fresh_id t =
        let id = t.next_fresh_id in 
        t.next_fresh_id <- id + 1 ;
        id

    let add t e = 
        let id = fresh_id t in
        Queue.push e t.elems ;
        id

    let to_array t = Utils.array_of_queue t.elems

end

module Dict =
struct

    (*  Invariant: 
        + For every i in [0,next_fresh_id)] and no other,
          `elems` maps i to an element *)
    type 'a t = {
        name_to_id : (string, int) Hashtbl.t ;
        mutable next_fresh_id : int ;
        mutable elems : 'a IntMap.t ;
        create_elem : unit -> 'a ;
    }

    let create create_elem = {
        name_to_id = Hashtbl.create 100 ;
        next_fresh_id = 0 ;
        elems = IntMap.empty ;
        create_elem ;
    }

    let fresh_id t =
        let id = t.next_fresh_id in 
        t.next_fresh_id <- id + 1 ;
        id

    (* Creates anonymous *)
    let new_anonymous t = 
        let id = fresh_id t in
        let elem = t.create_elem () in
        t.elems <- IntMap.add id elem t.elems ;
        id

    (* Creates a new element if needed *)
    let get t id =
        try IntMap.find id t.elems 
        with 
        Not_found ->
            let elem = t.create_elem () in
            t.elems <- IntMap.add id elem t.elems;
            elem

    (* Creates a fresh id if needed *)
    let id_of_name t name = 
        try Hashtbl.find t.name_to_id name
        with Not_found ->
            begin
                let id = fresh_id t in
                Hashtbl.add t.name_to_id name id ;
                ignore (get t id) ;
                id
            end

    let to_array t = 
        let n = t.next_fresh_id in
        try
            Array.init n (fun i -> IntMap.find i t.elems)
        with Not_found -> assert false

end



(*****************************************************************************)
(* Parse whole query to find constrained agent's types                       *)
(*****************************************************************************)

let clause_pattern = function
    | Ast.Event e -> e
    | Ast.First_after (e, _) -> e
    | Ast.Last_before (e, _) -> e

let constrained_agents_types q = 
    q.Ast.pattern |> List.fold_left (fun acc clause ->
        let pat = clause_pattern clause in
        pat.Ast.main_pattern |> List.fold_left (fun acc ag ->
            match ag.Ast.ag_constr with
            | None -> acc
            | Some id -> 
                let kind = ag.Ast.ag_kind in
                SMap.add id kind acc
        ) acc
    ) SMap.empty

let single_clause q = List.length q.Ast.pattern = 1

(*****************************************************************************)
(* Compilation environment                                                   *)
(*****************************************************************************)

type tmp_event = {
    tmp_ev_measures: measure PreArray.t ;
    tmp_main_pats: event_pattern Queue.t ;
    tmp_def_rels: defining_relation Queue.t ;
}

type tmp_agent = {
    mutable tmp_ag_kind : agent_kind option ;
}

type env = {
    query_agents : tmp_agent Dict.t ;
    query_events : tmp_event Dict.t ;
    model : Model.t ;
    model_signature : Signature.s ;
    constrained_agents_types : string SMap.t ;
    single_clause : bool ;
}

let create_env (model : Model.t) (q : Ast.query) = 
    let create_agent () = { tmp_ag_kind=None } in
    let create_event () = {
        tmp_ev_measures = PreArray.create () ;
        tmp_main_pats = Queue.create () ;
        tmp_def_rels = Queue.create () ;
    } in { 
        model ;
        model_signature = Model.signatures model ;
        query_agents = Dict.create create_agent ;
        query_events = Dict.create create_event ;
        constrained_agents_types = 
            constrained_agents_types q ;
        single_clause = single_clause q ;
    }

(*****************************************************************************)
(* Compile measures                                                          *)
(*****************************************************************************)

type some_expr = E : 'a expr -> some_expr

type some_expr_type = ST : 'a expr_type -> some_expr_type


let eval_ev_expr env cur_ev_id = function
    | Ast.Ev name -> Dict.id_of_name env.query_events name
    | Ast.This -> 
        begin match cur_ev_id with
        | None -> failwith "Bad usage of `this`."
        | Some id -> id 
    end
    
let eval_st_expr env cur_ev_id = function
    | Ast.Before ev_expr -> eval_ev_expr env cur_ev_id ev_expr, Before
    | Ast.After  ev_expr -> eval_ev_expr env cur_ev_id ev_expr, After

let register_measure in_action _cur_ev_id ev_id ev measure_descr ty = 
    let used_in_pattern = not in_action in
    let m_id = PreArray.add ev.tmp_ev_measures 
        { used_in_pattern ; measure_descr } in
    E (Measure (ev_id, m_id), ty)


let compile_event_measure env in_action cur_ev_id ev_expr m =
    let ev_id = eval_ev_expr env cur_ev_id ev_expr in
    let ev = Dict.get env.query_events ev_id in
    let measure_descr, ST ty = 
        match m with
        | Ast.Time -> Event_measure (Float, Time), ST Float
        | Ast.Rule -> Event_measure (String, Rule), ST String
        | Ast.Init_event -> Event_measure (Bool, Init_event), ST Bool in
    register_measure in_action cur_ev_id ev_id ev measure_descr ty


let tr_agent env ag_name = 
    Dict.id_of_name env.query_agents ag_name

let tr_agent_kind env s = 
    Signature.num_of_agent (Locality.dummy_annot s) env.model_signature 

let tr_site_name env ag_kind_id s = 
    Signature.num_of_site (Locality.dummy_annot s)
        (Signature.get env.model_signature ag_kind_id)

let tr_int_state env ag_id s_id st = 
    Signature.num_of_internal_state s_id (Locality.dummy_annot st)
        (Signature.get env.model_signature ag_id)

let tr_quark env (ag_name, site_name) = 
    let ag_id = tr_agent env ag_name in
    let ag_kind_name = 
        try SMap.find ag_name env.constrained_agents_types
        with Not_found -> failwith "Illegal agent usage." in
    let agent_kind = tr_agent_kind env ag_kind_name in
    let site_id = tr_site_name env agent_kind site_name in
    ((ag_id, agent_kind), site_id)


let compile_state_measure env in_action cur_ev_id st_expr m = 
    let ev_id , m_time = eval_st_expr env cur_ev_id st_expr in
    let ev = Dict.get env.query_events ev_id in

    let measure_descr, ST ty = 
        match m with
        | Ast.Nphos ag_name ->
            State_measure (m_time, Int, Nphos (tr_agent env ag_name)), ST Int 
        | Ast.Component ag_name ->
            State_measure (m_time, Agent_set, Component (tr_agent env ag_name)), ST Agent_set
        | Ast.Int_state quark ->
            let quark = tr_quark env quark in
            State_measure (m_time, String, Int_state quark), ST String
        | Ast.Snapshot -> State_measure (m_time, String, Snapshot), ST String
    in
   register_measure in_action cur_ev_id ev_id ev measure_descr ty


(*****************************************************************************)
(* Compile expressions                                                       *)
(*****************************************************************************)

let expr_not i = if i = 0 then 1 else 0

let expr_ty = snd

let expr_body = fst

type same_type = Same_type : 'a expr * 'a expr * 'a expr_type -> same_type

let same_type : type a b . a expr -> b expr -> same_type option =
fun lhs rhs ->
    match expr_ty lhs, expr_ty rhs with
    | Bool, Bool -> Some (Same_type (lhs, rhs, Bool))
    | Int, Int -> Some (Same_type (lhs, rhs, Int))
    | Float, Float -> Some (Same_type (lhs, rhs, Float))
    | String, String -> Some (Same_type (lhs, rhs, String))
    | Agent_set, Agent_set -> Some (Same_type (lhs, rhs, Agent_set))

    (* Implicit coercions *)
    | Int, Float -> 
        let lhs = (Unop (Unop float_of_int, lhs), Float) in
        Some (Same_type (lhs, rhs, Float))
    | Float, Int -> 
        let rhs = (Unop (Unop float_of_int, rhs), Float) in
        Some (Same_type (lhs, rhs, Float))

    | _ -> None

let int_of_bool = function
    | true -> 1
    | false -> 0


module IntSet = Utils.IntSet

let agent_set_ids s = IntSet.of_list (List.map fst (AgentSet.elements s))

let set_similarity s s' =
    let s = agent_set_ids s in
    let s' = agent_set_ids s' in
    let denom = IntSet.cardinal (IntSet.union s s') in
    if denom = 0 then 1.0 else
        let num = IntSet.cardinal (IntSet.inter s s') in
        float_of_int num /. float_of_int denom


(* Binary operators always have similarly typed arguments *)
let combine_binop : type a . Ast.binop -> a expr -> a expr -> some_expr =

    let arith : 
        type b . (int -> int -> int) -> (float -> float -> float) -> 
        b expr -> b expr -> some_expr =
        fun int_op float_op lhs rhs ->
        match expr_ty lhs with
        | Int -> E (Binop (lhs, Binop int_op, rhs), Int)
        | Float -> E (Binop (lhs, Binop float_op, rhs), Float) 
        | _ -> assert false 
    in

    let comparison :
        type b . (int -> int -> bool) -> (float -> float -> bool) ->
        b expr -> b expr -> some_expr =
        fun int_op float_op lhs rhs ->
        match expr_ty lhs with
        | Int -> 
            E (Binop (lhs, Binop int_op, rhs), Bool)
        | Float ->
            E (Binop (lhs, Binop float_op, rhs), Bool) 
        | _ -> assert false 
    in

    let boolop :
        type b . (bool -> bool -> bool) -> b expr -> b expr -> some_expr =
        fun op lhs rhs ->
        match expr_ty lhs with
        | Bool -> E (Binop (lhs, Binop op, rhs), Bool)
        | _ -> failwith "Boolean combinator is given non-boolean arguments." in

    fun op lhs rhs ->
        begin match op with
            | Ast.Eq -> E (Binop (lhs, Eq, rhs), Bool)
            | Ast.Similarity -> 
                begin match expr_ty lhs with
                | Agent_set -> E (Binop (lhs, Binop set_similarity, rhs), Float)
                | _ -> failwith "`similarity` expects agent sets as arguments."
                end
            | Ast.Add -> arith ( + ) ( +. ) lhs rhs 
            | Ast.Mul -> arith ( * ) ( *. ) lhs rhs
            | Ast.Sub -> arith ( - ) ( -. ) lhs rhs

            | Ast.Gt -> comparison ( > )  ( > )  lhs rhs
            | Ast.Ge -> comparison ( >= ) ( >= ) lhs rhs
            | Ast.Lt -> comparison ( < )  ( < )  lhs rhs
            | Ast.Le -> comparison ( <= ) ( <= ) lhs rhs

            | Ast.And -> boolop ( && ) lhs rhs
            | Ast.Or  -> boolop ( || ) lhs rhs
        end


let rec compile_expr env in_action cur_ev_id e = 
    match e with
    | Ast.Int_const i -> E (Const i, Int)
    | Ast.Float_const f -> E (Const f, Float)
    | Ast.String_const s -> E (Const s, String)
    | Ast.Unop (Ast.Not, arg_ast) ->
        let E arg = compile_expr env in_action cur_ev_id arg_ast in
        begin match expr_ty arg with
        | Bool -> E (Unop (Unop not, arg), Bool)
        | _ -> assert false
    end
    | Ast.Unop (Ast.Size, arg_ast) ->
        let E arg = compile_expr env in_action cur_ev_id arg_ast in
        begin match expr_ty arg with
        | Agent_set -> E (Unop (Unop Agent.SetMap.Set.size, arg), Int)
        | _ -> failwith "`size` can only be applied to sets of agents."
        end
    | Ast.Binop (lhs, op, rhs) ->
        let E lhs = compile_expr env in_action cur_ev_id lhs in
        let E rhs = compile_expr env in_action cur_ev_id rhs in
        begin match same_type lhs rhs with
        | Some (Same_type (lhs, rhs, _ty)) -> combine_binop op lhs rhs
        | None -> failwith "Type error."
        end
    | Ast.State_measure (st, m) ->
        compile_state_measure env in_action cur_ev_id st m
    | Ast.Event_measure (ev, m) ->
        compile_event_measure env in_action cur_ev_id ev m
    | Ast.Concat (lhs, rhs) ->
        let E lhs = compile_expr env in_action cur_ev_id lhs in
        let E rhs = compile_expr env in_action cur_ev_id rhs in
        E (Binop (lhs, Concat, rhs), Tuple)
    | Ast.Count_agents (ag_kinds, arg) ->
        let E arg = compile_expr env in_action cur_ev_id arg in
        begin match expr_ty arg with
        | Agent_set -> 
            let ags = List.map (tr_agent_kind env) ag_kinds in
            E (Unop (Count_agents ags, arg), Tuple)
        | _ -> failwith "`count` expects a set of agents as its second argument."
        end
    | Ast.Agent_id ag_name ->
        E (Agent_id (tr_agent env ag_name), Int)


(*****************************************************************************)
(* Compile mixture patterns                                                  *)
(*****************************************************************************)

let compile_mixture_pattern env ags = 

    let ags_array = Array.mapi (fun i x -> (i, x)) (Array.of_list ags) in

    let agents = ags_array |> Array.map (fun (_, ag) ->
        { agent_kind = tr_agent_kind env ag.Ast.ag_kind }) in

    let fold_sites f acc =
        ags_array |> Array.fold_left (fun acc (ag_id, ag) ->
            ag.Ast.ag_sites |> List.fold_left (fun acc s ->
                let ag_kind_id = agents.(ag_id).agent_kind in
                let site_id = tr_site_name env ag_kind_id s.Ast.site_name in
                f acc ag_id ag_kind_id ag site_id s
            ) acc 
        ) acc in

    let bonds : (int, site) Hashtbl.t = Hashtbl.create 10 in

    let tests = fold_sites (fun acc ag_id ag_kind_id _ag site_id ast_site ->
        let site = (ag_id, site_id) in
        let acc = match ast_site.Ast.site_int_test with
            | None -> acc
            | Some st -> 
                let st = tr_int_state env ag_kind_id site_id st in
                (Int_state_is (site, st)) :: acc in
        let acc = match ast_site.Ast.site_lnk_test with
            | None -> acc 
            | Some Ast.Free -> (Lnk_state_is (site, Free)) :: acc
            | Some (Ast.Bound b) ->
                begin try 
                    let dest = Hashtbl.find bonds b in
                    (Lnk_state_is (site, Bound_to dest)) :: 
                    (Lnk_state_is (dest, Bound_to site)) ::
                    acc
                with Not_found -> 
                    Hashtbl.add bonds b site ;
                    acc 
                end
            | Some (Ast.Bound_to_type (ag_kind, site_name)) ->
                let ag_kind = tr_agent_kind env ag_kind in
                let site_name = tr_site_name env ag_kind site_name in
                (Lnk_state_is (site, Bound_to_type (ag_kind, site_name))) :: acc
            | Some (Ast.Bound_to_any) -> (Lnk_state_is (site, Bound_to_any)) :: acc
        in acc ) [] in

    Hashtbl.reset bonds ;

    let mods = fold_sites (fun acc ag_id ag_kind_id _ag site_id ast_site ->
        let site = (ag_id, site_id) in
        let acc = match ast_site.Ast.site_int_mod with
            | None -> acc
            | Some st ->
                let st = tr_int_state env ag_kind_id site_id st in
                (Mod_int_state (site, st)) :: acc in
        let acc = match ast_site.Ast.site_lnk_mod with
            | None -> acc
            | Some Ast.Free -> (Mod_lnk_state (site, Free)) :: acc
            | Some (Ast.Bound b) ->
                begin try 
                    let dest = Hashtbl.find bonds b in
                    (Mod_lnk_state (site, Bound_to dest)) :: 
                    (Mod_lnk_state (dest, Bound_to site)) ::
                    acc
                with Not_found -> 
                    Hashtbl.add bonds b site ;
                    acc 
                end
            | Some (Ast.Bound_to_type (ag_kind, site_name)) ->
                let ag_kind = tr_agent_kind env ag_kind in
                let site_name = tr_site_name env ag_kind site_name in
                (Mod_lnk_state (site, Bound_to_type (ag_kind, site_name))) :: acc
            | Some (Ast.Bound_to_any) -> (Mod_lnk_state (site, Bound_to_any)) :: acc in
        acc ) [] in

    let mods = ags_array |> Array.fold_left (fun acc (ag_id, ag) ->
        match ag.Ast.ag_mod with
        | None -> acc
        | Some Ast.Create -> (Create ag_id) :: acc
        | Some Ast.Erase -> (Destroy ag_id) :: acc
    ) mods in

    let agent_constraints = ags_array |> Array.fold_left (fun acc (ag_id, ag) ->
        match ag.Ast.ag_constr with
        | None -> acc
        | Some name -> 
            let qid = Dict.id_of_name env.query_agents name in
            (Dict.get env.query_agents qid).tmp_ag_kind <- 
                Some agents.(ag_id).agent_kind ;
            IntMap.add qid ag_id acc
    ) IntMap.empty in

    { agents ; tests ; mods ; agent_constraints }



(*****************************************************************************)
(* Compile events                                                            *)
(*****************************************************************************)

let compile_rule_constraint env = function
    | Some (Ast.Rule rs) -> 
        Some (Rule (Utils.concat_map
            (fun r -> Model.nums_of_rule r env.model) rs))
    | Some (Ast.Obs s) -> Some (Obs s)
    | None -> None

let true_expr = (Const true, Bool)

let compile_with_clause env name_opt = function
    | None -> true_expr
    | Some wc ->
        begin match compile_expr env false name_opt wc with
            | E (e, Bool) -> (e, Bool)
            | _ -> failwith "A with clause should be of type `int`."
        end

let compile_event_pattern env pat =
    let cur_ev_id = 
        match pat.Ast.event_id with
        | None -> Dict.new_anonymous env.query_events
        | Some name -> Dict.id_of_name env.query_events name in
    
    let with_clause = compile_with_clause env (Some cur_ev_id) pat.Ast.with_clause in
    let rule_constraint = compile_rule_constraint env pat.Ast.rule_constraint in
    let main_pattern = compile_mixture_pattern env pat.Ast.main_pattern in
    cur_ev_id, {main_pattern; with_clause; rule_constraint}


let process_clauses env (tpat : Ast.clause list) = 
    tpat |> List.iter (function 
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
            Queue.push rel ev.tmp_def_rels
    )


let make_event i tmp_ev = { 
    event_id = i ;
    event_pattern = 
        begin match Utils.list_of_queue tmp_ev.tmp_main_pats with
        | [] -> None
        | [p] -> Some p
        | _ -> failwith "There can be at most one main clause for every event."
        end ;
    defining_rel = 
        begin match Utils.list_of_queue tmp_ev.tmp_def_rels with
        | [] -> None
        | [r] -> Some r
        | _ -> failwith "There can be at most one defining relation for an event."
        end ;
    measures = PreArray.to_array tmp_ev.tmp_ev_measures ;
    already_constrained_agents = [] ;
    captured_agents = [] ;
}

let make_agent {tmp_ag_kind} = 
    match tmp_ag_kind with
    | None -> failwith "Unbound agent identifier."
    | Some k -> k

let compile_trace_pattern env tpat = 
    process_clauses env tpat ;
    let events = Array.mapi make_event (Dict.to_array env.query_events) in
    let agents = Array.map make_agent (Dict.to_array env.query_agents) in

    (* Still uncomplete: we put a dummy value *)
    let traversal_tree = Tree ((-1), []) in
    { agents; events; traversal_tree; }



(*****************************************************************************)
(* Compile action                                                            *)
(*****************************************************************************)

let compile_action env when_clause = function
    | Ast.Print e ->
        let E e = compile_expr env true None e in
        begin match when_clause with
        | None -> Print e
        | Some (E (b, Bool)) -> If ((b, Bool), Print e)
        | Some (E (_, _)) -> 
            failwith "The when clause should be of type `bool`."
        end

(*****************************************************************************)
(* Compile queries                                                           *)
(*****************************************************************************)

let predecessor ev = 
    match ev.defining_rel with
    | None -> None
    | Some (First_after (pid, _) as rel) -> Some (rel, pid)
    | Some (Last_before (pid, _) as rel ) -> Some (rel, pid)

let compute_traversal_tree (q : query) = 
    let roots = Queue.create () in
    let succs = Hashtbl.create 100 in
     
    q.pattern.events |> Array.iteri (fun ev_id ev ->
        match predecessor ev with
        | None ->  Queue.push ev_id roots
        | Some (rel, pred_id) -> Hashtbl.add succs pred_id (rel, ev_id)
    ) ;
    
    let roots = Utils.list_of_queue roots in
    begin match roots with
    | [] -> failwith "No root was provided."
    | _ :: _ :: _ -> failwith "The query graph is not connected."
    | [ root_id ] -> 
        let rec build_tree i = 
            let children = 
                Hashtbl.find_all succs i
                |> List.map (fun (rel, j) -> (rel, build_tree j)) in
            Tree (i, children) in
        build_tree root_id
    end


let def_rel_pattern ev = 
    match ev.defining_rel with
    | None -> None
    | Some (First_after (_, p)) -> Some p
    | Some (Last_before (_, p)) -> Some p

let opt_pattern_constrained_agents = function
    | None -> []
    | Some p ->
        List.map fst (IntMap.bindings p.main_pattern.agent_constraints)


let constrained_agents (ev : event) =
    IntSet.of_list @@
        opt_pattern_constrained_agents (ev.event_pattern) @
        opt_pattern_constrained_agents (def_rel_pattern ev)


let schedule_agents_capture q = 
    let rec aux (Tree (i, children)) constrained =
        let ev = q.pattern.events.(i) in
        let ev_ags = constrained_agents ev in
        let acas, cas = 
            IntSet.partition (fun j -> IntSet.mem j constrained) ev_ags in
        let already_constrained_agents = IntSet.elements acas in
        let captured_agents = IntSet.elements cas in
        q.pattern.events.(i) <- 
            { ev with already_constrained_agents ; captured_agents } ;
        let constrained = IntSet.union constrained ev_ags in
        children |> List.iter (fun (_r, t) -> aux t constrained)
    in aux q.pattern.traversal_tree IntSet.empty


let schedule_execution (q : query) = 
    let traversal_tree = compute_traversal_tree q in
    let q = { q with pattern = { q.pattern with traversal_tree } } in
    schedule_agents_capture q ;
    q

let compile (model : Model.t) (q : Ast.query) = 
    let env = create_env model q in
    (* Compile the action first so that no measure is missing in the pattern *)
    let when_clause = Utils.map_option (compile_expr env true None) q.Ast.when_clause in
    let action = compile_action env when_clause q.Ast.action in
    let pattern = compile_trace_pattern env q.Ast.pattern in
    let title = q.Ast.query_name in
    let legend = q.Ast.legend in
    let every_clause = q.Ast.every_clause in
    schedule_execution { pattern ; action ; title ; legend ; every_clause }
