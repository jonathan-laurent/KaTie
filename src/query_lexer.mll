(*****************************************************************************)
(* Lexer                                                                     *)
(*****************************************************************************)

{

open Lexing
open Query_parser
open Tql_error

let keywords_list =
    [("match", MATCH); ("return", RETURN); ("do", RETURN); ("and", AND);
     ("with", WITH); ("last", LAST); ("first", FIRST); ("when", WHEN);
     ("before", BEFORE); ("after", AFTER);
     ("time", TIME); ("nphos", NPHOS); ("rule", RULE); ("count", COUNT);
     ("component", COMPONENT); ("dist", DIST); ("size", SIZE);
     ("query", QUERY); ("int_state", INT_STATE); ("similarity", SIMILARITY);
     ("every", EVERY); ("seconds", SECONDS); ("agent_id", AGENT_ID);
     ("snapshot", SNAPSHOT); ("init_event", INIT_EVENT); ("not", NOT);
     ("print_cc", PRINT_CC)]

let ktab =
    let t = Hashtbl.create 20 in
    keywords_list |> List.iter (fun (k, v) ->
        Hashtbl.add t k v) ;
    t

let kword_or_id s =
    try Hashtbl.find ktab s with Not_found -> ID s

let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }

}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let non_null_digit = ['1'-'9']

let integer = "0" | (non_null_digit digit*)



let float_exponent = ['E' 'e'] ['+' '-']? digit+

let float_with_dot = digit* "." (digit*)? float_exponent?

let float_without_dot = digit* float_exponent

let float_number = float_with_dot | float_without_dot



let ident = (letter | '_') (letter | '_' | digit)*

let space = [' ' '\t' '\r']


(* There seems to be a bug here as `[||]` is parsed as
   OP_SQPAR LOGIC_OR CL_SQPAR *)

rule token = parse
  | "'" ([^'\'']* as s) "'"  {STRING s}
  | "\"" ([^'"']* as s) "\"" {STRING s}
  | space {token lexbuf}
  | "\n" {new_line lexbuf; token lexbuf}
  | "/*" {comment true lexbuf}
  | "//" {comment false lexbuf}

  | "&&" {LOGIC_AND}
  | "||" {LOGIC_OR}

  | "(" {OP_PAR}
  | ")" {CL_PAR}
  | "{" {OP_CURL}
  | "}" {CL_CURL}
  | "[" {OP_SQPAR}
  | "]" {CL_SQPAR}
  | "|" {BAR}
  | "/" {SLASH}

  | "=" {EQ}
  | "#" {SHARP}
  | ":" {COLON}
  | "," {COMMA}
  | "." {DOT}
  | "_" {UNDERSCORE}

  | "<" {LT}
  | "<=" {LE}
  | ">" {GT}
  | ">=" {GE}
  | "+" {PLUS}
  | "-" {MINUS}
  | "*" {MULT}

  | float_number as s {FLOAT (float_of_string s)}
  | integer as s {INT (int_of_string s)}
  | ident as s {kword_or_id s}

  | eof {EOF}
  | _ { raise (error_at (lexeme_start_p lexbuf) Lexer_error)}


and comment multiline = parse
  | "\n" { new_line lexbuf ; if multiline then comment true lexbuf else token lexbuf }
  | "*/" { if multiline then token lexbuf else comment false lexbuf }
  | eof {EOF}
  | _ {comment multiline lexbuf}