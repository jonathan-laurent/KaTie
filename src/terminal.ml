(*****************************************************************************)
(* Utilities for the terminal CLI                                            *)
(*****************************************************************************)

type style = ANSITerminal.style

let no_color = ref false

let red, green, yellow, blue, cyan, magenta, bold =
  ANSITerminal.(red, green, yellow, blue, cyan, magenta, Bold)

let print s msg =
  (if !no_color then print_string msg else ANSITerminal.(print_string s msg)) ;
  flush_all ()

let println s msg = print s msg ; print s "\n"

let with_elapsed_time f () =
  let t = Sys.time () in
  let v = f () in
  (v, Sys.time () -. t)

let task ?(style = []) msg f =
  print style msg ;
  let v, elapsed = with_elapsed_time f () in
  println style (Fmt.str " (%.2fs)" elapsed) ;
  v

let party_emoji () = if !no_color then "" else " \u{1F389}"
