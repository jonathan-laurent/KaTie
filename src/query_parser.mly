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
%}

%token EOF QUERY
%token COLON DOT COMMA UNDERSCORE
%token EQ PLUS MINUS MULT GT GE LT LE
%token NOT
%token OP_PAR CL_PAR OP_SQPAR CL_SQPAR OP_CURL CL_CURL BAR
%token LINK INT_STATE

%token MATCH DO AND WITH LAST FIRST BEFORE AFTER

%token TIME NPHOS RULE COUNT COMPONENT DIST SIZE

%token <int> INT
%token <string> STRING
%token <string> ID

%nonassoc EQ GT GE LT LE

%left COMMA
%left MINUS PLUS
%left MULT

%start mixture_pat
%type <Query_ast.mixture_pat> mixture_pat

%start query 
%type <Query_ast.query> query

%start queries
%type <Query_ast.query list> queries

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

site:
  | name=ID { make_site name }
  | name=ID bond=lnk_state_attr { bond (make_site name) }
  | name=ID intst=int_state_attr { intst (make_site name) }
  | name=ID bond=lnk_state_attr intst=int_state_attr 
    { intst (bond (make_site name)) }
  | name=ID intst=int_state_attr bond=lnk_state_attr
    { intst (bond (make_site name)) }

lnk_state_attr:
  | LINK bond=lnk_state 
    { fun s -> { s with site_lnk_test = Some bond }  }
  | LINK OP_SQPAR bond=lnk_state CL_SQPAR 
    { fun s -> { s with site_lnk_mod = Some bond }  }

lnk_state:
  | DOT { Free }
  | bond=INT { Bound bond }
  | UNDERSCORE { Bound_to_any }
  | site_name=ID DOT agent_kind=ID 
    { Bound_to_type (agent_kind, site_name) }

int_state_attr:
  | INT_STATE st=ID 
    { fun s -> { s with site_int_test = Some st } }
  | INT_STATE OP_SQPAR st=ID CL_SQPAR 
    { fun s -> { s with site_int_mod = Some st } }


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

st_measure_annot:
  | { After This }
  | OP_SQPAR id=ID CL_SQPAR { After (Ev id) }
  | OP_SQPAR id=ID DOT CL_SQPAR { After (Ev id) }
  | OP_SQPAR DOT id=ID CL_SQPAR { Before (Ev id) }

ev_measure_annot:
  | { This }
  | OP_SQPAR id=ID CL_SQPAR { Ev id }

expr:
  | i=INT { Int_const i }
  | s=STRING { String_const s }
  | unop=unop e=expr { Unop (unop, e) }
  | SIZE OP_CURL e=expr CL_CURL { Unop (Size, e) }
  | lhs=expr op=binop rhs=expr { Binop (lhs, op, rhs) }
  | lhs=expr COMMA rhs=expr { Concat (lhs, rhs) }
  | COUNT OP_CURL agents=separated_list(COMMA, STRING) CL_CURL OP_CURL e=expr CL_CURL
    { Count_agents (agents, e) }
  | TIME ev_expr=ev_measure_annot
    { Event_measure (ev_expr, Time) }
  | RULE ev_expr=ev_measure_annot
    { Event_measure (ev_expr, Rule) }
  | NPHOS st_expr=st_measure_annot OP_CURL id=ID CL_CURL
    { State_measure (st_expr, Nphos id) }
  | COMPONENT st_expr=st_measure_annot OP_CURL id=ID CL_CURL
    { State_measure (st_expr, Component id) }


pattern: MATCH clauses=separated_list(AND, clause) { clauses }

clause:
  | evp=ev_pattern { Event evp }
  | FIRST evp=ev_pattern AFTER id=ID { First_after (evp, id) }
  | LAST evp=ev_pattern BEFORE id=ID { Last_before (evp, id) }

ev_name: id=ID COLON { id }

with_clause: WITH e=expr { e }

rule_constraint: 
  | rs=separated_nonempty_list(COMMA, STRING) { Rule rs }

ev_pattern: 
  event_id=option(ev_name) 
  OP_SQPAR rule_constraint=option(rule_constraint) BAR 
  main_pattern=mixture_pat
  BAR CL_SQPAR
  with_clause=option(with_clause)
  { {event_id; with_clause; main_pattern; rule_constraint} }

action: e=expr { Print e }

query_legend: OP_CURL arg=separated_list(COMMA, STRING) CL_CURL { arg }

query_header: 
  | {fun q -> q}
  | QUERY name=STRING legend=option(query_legend)
    {fun q -> {q with query_name = Some name ; legend}}

query: header=query_header pattern=pattern DO OP_CURL action=action CL_CURL
  { header {pattern; action; query_name=None; legend=None} }

queries: qs=list(query) EOF { qs }