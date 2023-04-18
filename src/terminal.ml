(*****************************************************************************)
(* Utilities for the terminal CLI                                            *)
(*****************************************************************************)

type style = ANSITerminal.style

(* Useful when performing print debugging *)
let disable_progress_bars = true

let enable_party_emoji = false

let no_color = ref false

let red, green, yellow, blue, cyan, magenta, bold =
  ANSITerminal.(red, green, yellow, blue, cyan, magenta, Bold)

let print s msg =
  (if !no_color then print_string msg else ANSITerminal.(print_string s msg)) ;
  flush_all ()

let println s msg = print s msg ; print s "\n"

let with_elapsed_time f =
  let t = Sys.time () in
  let v = f () in
  (v, Sys.time () -. t)

let task ?(style = []) msg f =
  print style msg ;
  let v, elapsed = with_elapsed_time f in
  println style (Fmt.str " (%.2fs)" elapsed) ;
  v

let party_emoji () =
  if enable_party_emoji && not !no_color then " \u{1F389}" else ""

(* Progress bars *)

type progress_bar_fields =
  {progress: int -> unit; final_info: unit -> string option}

type progress_bar = prefix:string -> progress_bar_fields

let progress_bar ~nsteps ~prefix =
  let processed = ref 0 in
  let lastp = ref (-1) in
  let progress n =
    processed := !processed + n ;
    let p = !processed * 100 / nsteps in
    if p > !lastp then (
      let msg = Fmt.str "%s (%d%%) \r" prefix p in
      if not disable_progress_bars then print [] msg ;
      lastp := p )
  in
  let final_info () = None in
  {progress; final_info}

let open_progress_bar ~step ~info ~prefix =
  let processed = ref 0 in
  let last = ref (-step) in
  let progress n =
    if !processed >= !last + step then (
      let msg = Fmt.str "%s (%s) \r" prefix (info !processed) in
      if not disable_progress_bars then print [] msg ;
      last := !processed ) ;
    processed := !processed + n
  in
  let final_info () = Some (info !processed) in
  {progress; final_info}

let with_progress_bar prefix bar f =
  let {progress; final_info} = bar ~prefix in
  let res, dt = with_elapsed_time (fun () -> f ~progress) in
  let info = match final_info () with None -> "" | Some i -> i ^ ", " in
  println [] (Fmt.str "%s (%s%.2fs)" prefix info dt) ;
  res
