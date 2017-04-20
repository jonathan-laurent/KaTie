open Format

type error_kind = 
    | Lexer_error
    | Parse_error

type error = error_kind * Lexing.position option

exception Error of error

let error_at pos ek = Error (ek, Some pos)

let error ek = Error (ek, None)

let print_error_kind f = function
    | Lexer_error -> fprintf f "Unknown symbol."
    | Parse_error -> fprintf f "Parse error."


let print_opt_pos f = function
    | None -> ()
    | Some pos -> fprintf f "[Line %d] " pos.Lexing.pos_lnum

let print_error f (kind, pos) = 
    fprintf f "@[<v>%a%a@]@." print_opt_pos pos print_error_kind kind