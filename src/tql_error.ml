(* User-facing errors raised by TQL *)

type error_kind =
    | Lexer_error
    | Parse_error
    | File_not_found of string
    (* Query compilation errors *)
    | No_root_event
    | Disconnected_query_graph
    (* Runtime errors *)
    | Agent_ambiguity

type error = {kind: error_kind; loc: Lexing.position option; query: string option}

exception User_error of error

let fail ?loc kind =
    let query = Log.get_current_query () in
    raise (User_error {kind; loc; query})

let error_message = function
    | Lexer_error -> ["Lexer error: an unknown symbol was encountered."]
    | Parse_error -> ["Parse error."]
    | File_not_found s -> [Log.fmt "File '%s' not found." s]
    (* | Event_ambiguity -> [
        "Ambiguity detected.";
        "Hint: make sure that a root event exists in your query such that mapping this event to a trace event determines one matching at most."] *)
    | Agent_ambiguity -> [
        "Ambiguity detected.";
        "Hint: make sure that a root event exists in your query such that mapping this event to a trace event determines one matching at most."]
    | No_root_event -> ["The query has no root event."]
    | Disconnected_query_graph -> ["The query's dependency graph is disconnected."]

let string_of_error {kind; loc; query} =
    let opt x f = match x with None -> [] | Some x -> [f x]  in
    let attributes =
        opt loc (fun pos -> Log.fmt "line %d" pos.Lexing.pos_lnum) @
        opt query (fun q -> q) in
    let attributes =
        match attributes with
        | [] -> ""
        | ss -> " (" ^ String.concat ", " ss ^ ")" in
    String.concat "\n" (("User error" ^ attributes ^ ":") :: error_message kind)