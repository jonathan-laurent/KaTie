(*****************************************************************************)
(* Utilities for the terminal CLI                                            *)
(*****************************************************************************)

type style = ANSITerminal.style

let no_color = ref false

let red, green = ANSITerminal.(red, green)

let println s msg =
  if !no_color then print_endline msg
  else ANSITerminal.(print_string s (msg ^ "\n"))

let party_emoji () = if !no_color then "" else " \u{1F389}"
