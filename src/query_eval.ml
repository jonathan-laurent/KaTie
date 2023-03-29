(*****************************************************************************)
(* Simple query interpreter                                                  *)
(*****************************************************************************)

open Utils
open Query
open Streaming
open Matchings

(*****************************************************************************)
(* Types                                                                     *)
(*****************************************************************************)

type pm_id = int

type event_recorder = {
    recorder_status : recorder_status ;
    mutable cache : (ev_matchings History.t) ValMap.t ;
    mutable subscribed : (pm_id list) ValMap.t ;
}

and recorder_status =
    | Disabled
    | Enabled
    | Root

type partial_matching = {
    partial_matching_id : int ;
    constrained_agents : int IntMap.t ;
    matched_events : ev_matching IntMap.t ;
    watched : (matching_tree) IntMap.t ;
}

type env = {
    query : query ;
    model : Model.t ;
    recorders : event_recorder array ;
    mutable partial_matchings : (partial_matching branchings_memory) IntMap.t ;
    mutable next_fresh_id: int ;
    mutable last_root_matching_time: float ;
}

and 'a branchings_memory =
    | Elem of 'a
    | Branched of int list


(*****************************************************************************)
(* Debug pp                                                                  *)
(*****************************************************************************)

module Dbg = struct

open Debug_pp
open Format

let pp_event_matching_common f em =
    fprintf f "ev-id-in-query: %d@;" em.ev_id_in_query ;
    fprintf f "ev-id-in-trace: %d@;" em.ev_id_in_trace ;
    fprintf f "indexing-ag-matchings: %a@;"
        (pp_list_inline pp_int) em.indexing_ag_matchings

let pp_event_matching_specific f em =
    fprintf f "new-ag-matchings: %a@;"
        (pp_list_inline pp_int) em.new_ag_matchings

let pp_event_matching_specific_only f m = pp_event_matching_specific f m.specific

let pp_event_matching f m =
    pp_event_matching_common f m.common ;
    pp_event_matching_specific f m.specific

let pp_event_matchings f ems =
    pp_event_matching_common f ems.common_to_all ;
    List.iter (pp_event_matching_specific_only f) ems.matchings

let pp_partial_matching f pm =
    fprintf f "@[<v>" ;
    dbox f (asprintf "Partial matching [%d]" pm.partial_matching_id) ;
    box f "Constrained agents" ;
    pp_int_map (pp_int) f pm.constrained_agents ;
    box f "Matched events" ;
    pp_int_map pp_event_matching f pm.matched_events ;
    box f "Watched events" ;
    pp_int_map pp_tree f pm.watched ;
    pp_dline f ;
    fprintf f "@]"

let pp_complete_matching f cm =
    fprintf f "@[<v>" ;
    fprintf f "Agents: %a@;" (pp_array pp_int) cm.cm_agents ;
    cm.cm_events |> Array.iter (fun em ->
        fprintf f "%a@;" pp_event_matching em ;
    ) ;
    fprintf f "@]"

let pp_complete_matchings f cms =
    cms |> Array.iter (fun cm ->
        fprintf f "%a" pp_complete_matching cm ;
        pp_dline f ;
    )


end

(*****************************************************************************)
(* Simple operations on those types                                          *)
(*****************************************************************************)

(* Utils on queries *)

let number_of_events q =
    Array.length q.pattern.events

let number_of_agents q =
    Array.length q.pattern.agents

let query_root_event q =
    let Tree (root_id, _) = q.pattern.traversal_tree in
    root_id



(* Subscribtions *)

let get_subscribtions env (ev_id, v) =
    let r = env.recorders.(ev_id) in
    try ValMap.find v r.subscribed
    with Not_found -> []

let pop_subscribtions env (ev_id, v) =
    let r = env.recorders.(ev_id) in
    let ss = get_subscribtions env (ev_id, v) in
    r.subscribed <- ValMap.add v [] r.subscribed ;
    ss

let subscribe env (ev_id, v, pm_id) =
    let r = env.recorders.(ev_id) in
    let ss = get_subscribtions env (ev_id, v) in
    r.subscribed <- ValMap.add v (pm_id :: ss) r.subscribed



(* Cache *)

let store_in_cache env ev_id (ms : ev_matchings) =
    let reco = env.recorders.(ev_id) in
    let v = ms.common_to_all.indexing_ag_matchings in
    let hist =
        try ValMap.find v reco.cache
        with Not_found -> History.empty in
    let hist = History.add ms.common_to_all.ev_id_in_trace ms hist in
    reco.cache <- ValMap.add v hist reco.cache

let access_cache f env ev_id v ref_step_id =
    let cache = env.recorders.(ev_id).cache in
    try map_option snd (f ref_step_id (ValMap.find v cache))
    with Not_found -> None

let first_after_in_cache = access_cache History.first_after
let last_before_in_cache = access_cache History.last_before



(* Valuations *)

let indexing_ag_valuation env ev_id pm =
    let val_ags = env.query.pattern.events.(ev_id).already_constrained_agents in
    let find_valuation ag =
        try
            IntMap.find ag pm.constrained_agents
        with Not_found -> failwith (Format.sprintf "No mapping found for agent %d." ag) in
    List.map find_valuation val_ags

let valuation_to_mapping ag_pm_labels v =
    let rec zip l l' =
        match l, l' with
        | [], [] -> []
        | x::xs, y::ys -> (x, y) :: zip xs ys
        | _ -> assert false in
    int_map_from_list (zip ag_pm_labels v)



(* Partial matchings *)

let is_watched ev_id pm =
    try ignore (IntMap.find ev_id pm.watched) ; true
    with Not_found -> false

let update_constrained_agents ev m pm =
    let constrained_agents =
        Utils.update_int_map
            pm.constrained_agents
            (valuation_to_mapping ev.captured_agents m.specific.new_ag_matchings) in
    { pm with constrained_agents }


(* Complete matchings *)

let cm_get_agent_id cm qid =
    try Some (cm.cm_agents.(qid))
    with _ -> assert false

(* Returns [None] if not cached and [Some None] if cached but errored. *)
let cm_get_measure cm ev_id i =
    IntMap.find_opt i cm.cm_events.(ev_id).specific.recorded_measures

let cm_set_measure cm ev_id i v =
    let ev = cm.cm_events.(ev_id) in
    let recorded_measures = IntMap.add i v ev.specific.recorded_measures in
    cm.cm_events.(ev_id) <- {ev with specific={ev.specific with recorded_measures}}


(* Environment *)

let fresh_id env =
    let id = env.next_fresh_id in
    env.next_fresh_id <- id + 1 ;
    id

let get_event env ev_id = env.query.pattern.events.(ev_id)

let recorder_status env ev_id =
    env.recorders.(ev_id).recorder_status

let init_env model query =
    { query ; model ;
      recorders = Array.init (number_of_events query) (fun i ->
        let status = if i = query_root_event query then Root else Enabled in
        { recorder_status = status ;
          cache = ValMap.empty ;
          subscribed = ValMap.empty ;
        }) ;
      partial_matchings = IntMap.empty ;
      next_fresh_id = 0 ;
      last_root_matching_time = neg_infinity }



(*****************************************************************************)
(* Main procedure                                                            *)
(*****************************************************************************)

let zip_with_fresh env pm l =
    match l with
    | [] -> []
    | [x] -> [(pm, x)]
    | l ->
        begin
        l |> List.map (fun x ->
            let id = fresh_id env in
            ({ pm with partial_matching_id = id }, x) )
        end

(*  Does the following:
    + (Check that the registration is legitimate: at least one matching)
    + Update the `matched_events` map
    + Look for the Last_before children of `ev` and add them
    + Add the First_after children to the watch list
    + Suscribe to the recorder for these children

    You have to check that the event is watched indeed before calling. *)

let rec process_matching env subs children (pm, m) =

    let i = m.common.ev_id_in_query in
    let pm = { pm with matched_events = IntMap.add i m pm.matched_events } in
    let pm = update_constrained_agents (get_event env i) m pm in

    let process_child pm (rel, (Tree (j, _) as t)) =
        let v = indexing_ag_valuation env j pm in
        match rel with
        | First_after (i', _) -> assert (i = i') ;
            begin match first_after_in_cache env j v m.common.ev_id_in_trace with
                | Some ms' ->
                    let pm = {pm with watched = IntMap.add j t pm.watched} in
                    process_matchings env subs pm ms'
                | None ->
                    Queue.push (j, v, pm.partial_matching_id) subs;
                    let pm = {pm with watched = IntMap.add j t pm.watched} in
                    [ pm ]
            end
        | Last_before (i', _) -> assert (i = i') ;
            begin match last_before_in_cache env j v m.common.ev_id_in_trace with
            | Some ms' ->
                let pm = {pm with watched = IntMap.add j t pm.watched} in
                process_matchings env subs pm ms'
            | None -> []
            end
    in

    Utils.monadic_fold process_child pm children

and process_matchings env subs pm ms =
    try
        let ev_id = ms.common_to_all.ev_id_in_query in
        (* Get the ctree an remove the current agent from the watched list *)
        let Tree (i, children) = IntMap.find ev_id pm.watched in
        let pm = { pm with watched = IntMap.remove ev_id pm.watched } in
        assert (ev_id = i) ;

        (* We branch! *)
        let branches = zip_with_fresh env pm ms.matchings in
        List.concat (List.map (process_matching env subs children) branches)

    (* Event [ev_id] was not watched: we do nothing *)
    with Not_found -> [ pm ]



let rec update_partial_matching env ms pm_id =
    (*  If the mentioned partial matching has been branched,
        redirect the call to its children *)
    try match IntMap.find pm_id env.partial_matchings with
        | Elem pm ->
            begin
                let subscribtions = Queue.create () in
                let pms = process_matchings env subscribtions pm ms in
                let branched = List.map (fun pm' -> pm'.partial_matching_id) pms in
                env.partial_matchings <-
                    IntMap.add pm_id (Branched branched) env.partial_matchings ;
                pms |> List.iter (fun pm' ->
                    env.partial_matchings <-
                        IntMap.add pm'.partial_matching_id (Elem pm') env.partial_matchings
                ) ;
                Queue.iter (subscribe env) subscribtions
            end
        | Branched ids -> List.iter (update_partial_matching env ms) ids
    with Not_found -> assert false

let new_partial_matching env root_id ms =
    let id = fresh_id env in
    let pm =
        { partial_matching_id = id ;
          constrained_agents = IntMap.empty ;
          matched_events = IntMap.empty ;
          watched = IntMap.singleton root_id (env.query.pattern.traversal_tree) ;
        } in
    env.partial_matchings <- IntMap.add id (Elem pm) env.partial_matchings ;
    update_partial_matching env ms id


let is_delay_respected env cur_time =
    match env.query.every_clause with
    | None -> true
    | Some delta ->
        cur_time -. env.last_root_matching_time >= delta


let first_pass_process_step query window env =
    query.pattern.events |> Array.iteri (fun ev_id ev ->
        match recorder_status env ev_id with
        | Disabled -> ()
        | Enabled ->
          begin
            match Event_matcher.match_event ev window with
            | Some ms ->
                store_in_cache env ev_id ms ;
                let subscribtions = pop_subscribtions env
                    (ev_id, ms.common_to_all.indexing_ag_matchings) in
                List.iter (update_partial_matching env ms) subscribtions
            | None -> ()
          end
        | Root ->
          begin
            match Event_matcher.match_event ev window with
            | Some ms ->
                let cur_time = ms.common_to_all.ev_time in
                if is_delay_respected env cur_time then
                begin
                    new_partial_matching env ev_id ms ;
                    env.last_root_matching_time <- cur_time
                end
            | None -> ()
          end
    ) ;
    env


let extract_complete_matchings env =

    let finalize pm =
        try
            let cm_agents = Array.init (number_of_agents env.query) (fun i ->
              IntMap.find i pm.constrained_agents) in
            let cm_events = Array.init (number_of_events env.query) (fun i ->
              IntMap.find i pm.matched_events) in
            let matched_step_ids = cm_events
                |> Array.map (fun evm -> evm.common.ev_id_in_trace)
                |> Array.to_list in
            let cm_last_matched_step_id = Utils.list_maximum matched_step_ids in
            {cm_agents ; cm_events ; cm_last_matched_step_id}
        with Not_found ->
            begin
                print_endline "\nUnable to finalize a partial matching:";
                Dbg.pp_partial_matching Format.std_formatter pm ;
                print_endline "";
                assert false
            end in

    let process_pm _ pm acc =
        match pm with
            | Branched _ -> acc
            | Elem pm ->
                (* Printf.printf "%d" (IntMap.cardinal pm.watched) ; *)
                if IntMap.is_empty pm.watched then finalize pm :: acc
                else acc in

    Array.of_list (IntMap.fold process_pm env.partial_matchings [])


(*****************************************************************************)
(* Second pass                                                               *)
(*****************************************************************************)

(*  Forget all the measures taken for a given matching so as to free memory.
    This can be called safely after the corresponding action has been executed.
*)
let free_cm_memory (cm : complete_matching) =
    for i = 0 to Array.length cm.cm_events - 1 do
        let ev = cm.cm_events.(i) in
        cm.cm_events.(i) <-
            {ev with specific={ev.specific with
                recorded_measures = IntMap.empty}}
    done

(* Read a measure in the cache if it is cached or compute it using
   the current trace window otherwise *)
let read_or_take_measure ?uuid model query ag_matchings window cm =
    fun (ev_id, measure_id) ->
    match cm_get_measure cm ev_id measure_id with
    | Some x -> x
    | None ->
        let ev = query.pattern.events.(ev_id) in
        let measure = ev.measures.(measure_id) in
        Measures.take_measure ?uuid model ag_matchings window measure

let execute_action q fmt read_measure cm =
    let read_id = cm_get_agent_id cm in
    let rec aux = function
        | Print e ->
            let _ = Expr_eval.eval_expr_to_value read_measure read_id e |> map_option (fun v ->
                Format.fprintf fmt "%a@;" Expr_eval.print_value v ) in
            ()
        | If (cond, action) ->
            begin match Expr_eval.eval_expr read_measure read_id cond with
            | Some b -> if b then aux action
            | None -> failwith "Invalid conditional."
            end
    in aux q.action ; free_cm_memory cm


(* Initialize the accumulator state for the second pass,
   which consists in a list of [step_id, matching_id, event_id] triples sorted
   by increasing [step_id] *)
let prepare_second_pass cms =
    let triple_compare_on_first (x, _, _) (x', _, _) = compare x x' in
    let q = Queue.create () in
    cms |> Array.iteri (fun m_id m ->
        m.cm_events |> Array.iteri (fun ev_id ev ->
            let step_id = ev.common.ev_id_in_trace in
            Queue.push (step_id, m_id, ev_id) q
        ));
    List.sort triple_compare_on_first (Utils.list_of_queue q)

(* Take all measures corresponding to an event and stores the result using
   a provider setter. *)
let take_all_measures ?uuid model ag_matchings window set_measure ev =
    ev.measures |> Array.iteri (fun i measure ->
        let v = Measures.take_measure ?uuid model ag_matchings window measure in
        set_measure i v)

let second_pass_process_step
    ?(uuid : int option)
    ~(matchings_processed : int -> unit)
    (model : Model.t)
    (query : Query.query)
    (fmt : Format.formatter)
    (cms : complete_matching array) =

    fun (window : window) remaining ->
        let rec process = function
            | [] -> [] (* There's nothing interesting in the trace anymore *)
            | (step_id, m_id, ev_id) :: remaining
                when step_id = window.step_id ->
                (* Something interesting happens *)
                begin
                    let cm = cms.(m_id) in
                    matchings_processed(1);
                    let final_step = step_id = cm.cm_last_matched_step_id in
                    if final_step then begin
                        (* Non-cached measures are computed using the current window. *)
                        let read_or_take_measure =
                            read_or_take_measure ?uuid model query cm.cm_agents window cm in
                        execute_action query fmt read_or_take_measure cm
                    end else begin
                        (* We cache measures now since the result will
                           only be used later (when the action is performed). *)
                        take_all_measures
                            ?uuid model cm.cm_agents window
                            (cm_set_measure cm ev_id)
                            query.pattern.events.(ev_id);
                    end;
                    (* We proceed with our todo list *)
                    process remaining
                end
            | remaining -> remaining in
        process remaining


(*****************************************************************************)
(* Eval function                                                             *)
(*****************************************************************************)

let print_legend (q, fmt) =
    match q.legend with
    | None -> ()
    | Some ls ->
        let labels = List.map (fun l -> "'" ^ l ^ "'") ls in
        Format.fprintf fmt "%s@;" (String.concat ", " labels)


let eval
    ?(uuid : int option)
    (m : Model.t)
    (q : Query.query)
    (fmt : Format.formatter)
    (trace_file : string)
    : unit =

    let env = init_env m q in
    ignore @@ Streaming.fold_trace
        ~update_ccs:true
        ~compute_previous_states:true
        trace_file
        (first_pass_process_step q)
        env ;
    let cms = extract_complete_matchings env in
    let acc = prepare_second_pass cms in
    let matchings_processed(n) = () in
    Format.fprintf fmt "@[<v>" ;
    ignore @@ Streaming.fold_trace
        ~update_ccs:true
        ~compute_previous_states:true
        trace_file
        (second_pass_process_step ?uuid ~matchings_processed m q fmt cms)
        acc ;
    Format.fprintf fmt "@]"

let progress_bar msg nsteps =
    let processed = ref 0 in
    let lastp = ref (-1) in
    fun n -> begin
        processed := !processed + n;
        let p = (!processed * 100) / nsteps in
        if p > !lastp then begin
            Printf.printf "%s: %d%% \r" msg p;
            flush(stdout);
            lastp := p
        end
    end

let open_progress_bar msg step =
    let processed = ref 0 in
    let last = ref (-step) in
    fun n -> begin
        if !processed >= !last + step then begin
            Printf.printf "%s: %d \r" msg !processed;
            flush(stdout);
            last := !processed
        end;
        processed := !processed + n;
    end

let eval_queries
    ?(skip_init_events=false)
    ?(uuid : int option)
    (m : Model.t)
    (qs : (query * Format.formatter) list)
    (trace_file : string)
    : unit =

    let queries = Array.of_list (List.map fst qs) in
    let fmts = Array.of_list (List.map snd qs) in
    let envs = Array.map (init_env m) queries in

    let event_processed = open_progress_bar "Events processed" 10_000 in
    let step1 window () = begin
        event_processed 1 ;
        queries |> Array.iteri (fun i q ->
            ignore (first_pass_process_step q window envs.(i))
        )
    end in

    print_endline "Finding matchings..." ;

    ignore @@ Streaming.fold_trace
        ~update_ccs:true
        ~compute_previous_states:true
        ~skip_init_events
        trace_file
        step1
        () ;

    let cms = Array.map extract_complete_matchings envs in
    let accs = Array.map prepare_second_pass cms in

    List.iter print_legend qs ;

    let n_matchings = Utils.sum_array (Array.map (Array.length) cms) in
    (* cms |> Array.iter (Dbg.pp_complete_matchings Format.std_formatter) ; *)
    print_newline ();
    Printf.printf "Possible matchings found: %d\n" n_matchings;
    print_endline "Executing actions..." ;

    (* Progress bar support  *)
    let matchings_processed = progress_bar "Matchings processed" n_matchings in

    let step2 window () =
    queries |> Array.iteri (fun i q ->
        accs.(i) <- second_pass_process_step
            ?uuid ~matchings_processed m q fmts.(i) cms.(i) window accs.(i)
    ) in

    ignore @@ Streaming.fold_trace
        ~update_ccs:true
        ~compute_previous_states:true
        ~skip_init_events
        trace_file
        step2
        ();
    print_newline ()
