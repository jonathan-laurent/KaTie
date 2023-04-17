(*****************************************************************************)
(* Parser                                                                    *)
(*****************************************************************************)

%{
open Query_ast

let make_site site_name = {
    site_name ;
    site_lnk_test = None ;
    site_lnk_mod  = None ;
    site_int_test = None ;
    site_int_mod  = None }

let make_agent ag_mod id1 id2_opt ag_sites =
  match id2_opt with
  | None ->
    { ag_constr = None ; ag_kind = id1 ; ag_mod ; ag_sites}
  | Some id2 ->
    { ag_constr = Some id1 ; ag_kind = id2 ; ag_mod ; ag_sites}

let parse_rule_constraint_disjunct l =
  if String.equal l Trace_util.init_label then Init else Rule l
%}


%token EOF QUERY
%token COLON DOT COMMA UNDERSCORE
%token EQ PLUS MINUS MULT GT GE LT LE
%token NOT LOGIC_AND LOGIC_OR
%token OP_PAR CL_PAR OP_SQPAR CL_SQPAR OP_CURL CL_CURL BAR
%token SLASH SHARP
%token MATCH RETURN AND WITH LAST FIRST BEFORE AFTER WHEN
%token TIME NPHOS RULE COUNT COMPONENT DIST SIZE PRINT_CC DEBUG_EVENT
%token INT_STATE SIMILARITY AGENT_ID
%token EVERY SECONDS
%token SNAPSHOT

%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <string> ID

%left COMMA
%left LOGIC_AND LOGIC_OR
%nonassoc EQ GT GE LT LE
%left MINUS PLUS
%left MULT
%nonassoc NOT

%type <Query_ast.mixture_pat> mixture_pat

%start single_query
%type <Query_ast.t> single_query

%start queries
%type <Query_ast.t list> queries

%%

mixture_pat: agents=separated_list(COMMA, agent) { agents }

/* Defining agent's syntax is a bit tricky. */

ag_mod:
	| PLUS { Create }
	| MINUS { Erase }

agent_id2_opt: COLON ag_kind=ID { ag_kind }

agent_sites:
  | OP_PAR ag_sites=separated_list(COMMA, site) CL_PAR { ag_sites }
  | { [] }

agent:
  ag_mod=option(ag_mod)
  id1=ID
  id2_opt=option(agent_id2_opt)
  ag_sites=agent_sites
  { make_agent ag_mod id1 id2_opt ag_sites }

string_or_int:
  | name=ID { name }
  | i=INT { string_of_int i }

site:
  | name=string_or_int { make_site name }
  | name=string_or_int bond=lnk_state_attr { bond (make_site name) }
  | name=string_or_int intst=int_state_attr { intst (make_site name) }
  | name=string_or_int bond=lnk_state_attr intst=int_state_attr
    { intst (bond (make_site name)) }
  | name=string_or_int intst=int_state_attr bond=lnk_state_attr
    { intst (bond (make_site name)) }

lnk_state_attr:
  | OP_SQPAR bond=lnk_state option(SLASH) CL_SQPAR
    { fun s -> { s with site_lnk_test = Some bond } }
  | OP_SQPAR option(SHARP) SLASH bond=lnk_state CL_SQPAR
    { fun s -> { s with site_lnk_mod = Some bond } }
  | OP_SQPAR pre=lnk_state SLASH post=lnk_state CL_SQPAR
    { fun s -> { s with
      site_lnk_test = Some pre ;
      site_lnk_mod = Some post } }

lnk_state:
  | DOT { Free }
  | bond=INT { Bound bond }
  | UNDERSCORE { Bound_to_any }
  | site_name=ID DOT agent_kind=ID
    { Bound_to_type (agent_kind, site_name) }

int_state_attr:
  | OP_CURL st=string_or_int option(SLASH) CL_CURL
    { fun s -> { s with site_int_test = Some st } }
  | OP_CURL option(SHARP) SLASH st=string_or_int CL_CURL
    { fun s -> { s with site_int_mod = Some st } }
  | OP_CURL pre=string_or_int SLASH post=string_or_int CL_CURL
    { fun s -> { s with
      site_int_test = Some pre ;
      site_int_mod = Some post } }


%inline unop: NOT { Not }

%inline binop:
  | EQ { Eq }
  | PLUS { Add }
  | MINUS { Sub }
  | MULT { Mul }
  | GT { Gt }
  | GE { Ge }
  | LT { Lt }
  | LE { Le }
  | LOGIC_AND { And }
  | LOGIC_OR { Or }

st_measure_annot:
  | { After This }
  | OP_SQPAR id=ID CL_SQPAR { After (Ev id) }
  | OP_SQPAR id=ID DOT CL_SQPAR { After (Ev id) }
  | OP_SQPAR DOT id=ID CL_SQPAR { Before (Ev id) }

ev_measure_annot:
  | { This }
  | OP_SQPAR id=ID CL_SQPAR { Ev id }

quark: ag_id=ID DOT site_name=ID { (ag_id, site_name) }

expr:
  | OP_PAR e=expr CL_PAR { e }
  | i=INT { Int_const i }
  | f=FLOAT { Float_const f }
  | s=STRING { String_const s }
  | unop=unop e=expr { Unop (unop, e) }
  | SIZE OP_CURL e=expr CL_CURL { Unop (Size, e) }
  | lhs=expr op=binop rhs=expr { Binop (lhs, op, rhs) }
  | lhs=expr COMMA rhs=expr { Concat (lhs, rhs) }
  | COUNT OP_CURL agents=separated_list(COMMA, STRING) CL_CURL OP_CURL e=expr CL_CURL
    { Count_agents (agents, e) }
  | SIMILARITY OP_CURL e1=expr CL_CURL OP_CURL e2=expr CL_CURL
    { Binop (e1, Similarity, e2) }
  | DIST { Tql_error.(fail (Unimplemented "'dist'"))}
  | TIME ev_expr=ev_measure_annot
    { Event_measure (ev_expr, Time) }
  | RULE ev_expr=ev_measure_annot
    { Event_measure (ev_expr, Rule) }
  | NPHOS st_expr=st_measure_annot OP_CURL id=ID CL_CURL
    { State_measure (st_expr, Nphos id) }
  | COMPONENT st_expr=st_measure_annot OP_CURL id=ID CL_CURL
    { State_measure (st_expr, Component id) }
  | PRINT_CC st_expr=st_measure_annot OP_CURL id=ID CL_CURL
    { State_measure (st_expr, Print_cc id) }
  | INT_STATE st_expr=st_measure_annot OP_CURL quark=quark CL_CURL
    { State_measure (st_expr, Int_state quark) }
  | SNAPSHOT st_expr=st_measure_annot
    { State_measure (st_expr, Snapshot) }
  | AGENT_ID OP_CURL id=ID CL_CURL { Agent_id id }
  | DEBUG_EVENT ev_expr=ev_measure_annot
    {Event_measure (ev_expr, Debug_event)}



pattern: MATCH clauses=separated_list(AND, clause) { clauses }

clause:
  | evp=ev_pattern { Event evp }
  | FIRST evp=ev_pattern AFTER id=ID { First_after (evp, id) }
  | LAST evp=ev_pattern BEFORE id=ID { Last_before (evp, id) }

ev_name: id=ID COLON { id }

with_clause: WITH e=expr { e }

rule_constraint_disjunct: d=STRING { parse_rule_constraint_disjunct d }

rule_constraint: c=separated_nonempty_list(BAR, rule_constraint_disjunct) { c }

ev_pattern:
  | event_id=option(ev_name)
    OP_CURL rule_constraint=option(rule_constraint)
    main_pattern=mixture_pat
    CL_CURL
    with_clause=option(with_clause)
    { {event_id; with_clause; main_pattern; rule_constraint} }
  | id=ID with_clause=option(with_clause)
    { {event_id = Some id; with_clause; main_pattern = []; rule_constraint = None} }

action: e=expr { Print e }

query_legend: OP_CURL arg=separated_list(COMMA, STRING) CL_CURL { arg }

when_clause: WHEN e=expr { e }

float_or_int:
  | f=FLOAT { f }
  | i=INT { float_of_int i }

every_clause: EVERY f=float_or_int SECONDS { f }

query:
  QUERY query_name=STRING legend=option(query_legend)
  pattern=pattern
  every_clause=option(every_clause)
  when_clause=option(when_clause)
  RETURN action=action
  { {query_name; legend; pattern; when_clause; action; every_clause} }

single_query: q=query EOF { q }

queries: qs=list(query) EOF { qs }