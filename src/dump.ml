(*****************************************************************************)
(* Pretty-printing utilities for debugging                                   *)
(*****************************************************************************)

let dump_ag_lid query ~lid = query.Query.pattern.agents.(lid).agent_name

let dump_ev_lid query ~lid =
  query.Query.pattern.events.(lid).event_name |> Option.value ~default:"?"

let dump_agent_mapping query ~lid ~gid =
  Fmt.str "%s:%d" (dump_ag_lid query ~lid) gid

let dump_event_mapping query ~lid ~gid =
  Fmt.str "%s:%d" (dump_ev_lid query ~lid) gid

let dump_agents_mapping_list query ~lids ~gids =
  List.map2 (fun lid gid -> dump_agent_mapping query ~lid ~gid) lids gids
  |> String.concat " "

let dump_events_mapping_list query ~lids ~gids =
  List.map2 (fun lid gid -> dump_event_mapping query ~lid ~gid) lids gids
  |> String.concat " "

let dump_other_constrained query ev other_constrained =
  dump_agents_mapping_list query ~lids:ev.Query.other_constrained_agents
    ~gids:other_constrained

let dump_link query ev link =
  dump_agents_mapping_list query ~lids:ev.Query.link_agents ~gids:link

(* Execution path *)

let dump_execution_path query path =
  let ag_name lid = dump_ag_lid query ~lid in
  path
  |> List.map (fun ev_lid ->
         let ev = query.Query.pattern.events.(ev_lid) in
         let ev_name = dump_ev_lid query ~lid:ev_lid in
         let link = List.map ag_name ev.link_agents in
         let other_constrained = List.map ag_name ev.other_constrained_agents in
         Fmt.str "%s(%s->%s)" ev_name (String.concat "," link)
           (String.concat "," other_constrained) )
  |> String.concat " "
