(*****************************************************************************)
(* Utilities for the terminal CLI                                            *)
(*****************************************************************************)

type style = ANSITerminal.style

let no_color = ref false

let red, green, yellow, blue, cyan, magenta, bold =
  ANSITerminal.(red, green, yellow, blue, cyan, magenta, Bold)

let println s msg =
  if !no_color then print_endline msg
  else (
    ANSITerminal.(print_string s (msg ^ "\n")) ;
    flush_all () )

let party_emoji () = if !no_color then "" else " \u{1F389}"
