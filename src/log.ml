(*****************************************************************************)
(* KaTie's Custom Logging Module                                             *)
(*****************************************************************************)

module Smap = Utils.StringMap

let current_query = ref None

let profiling_enabled = ref true

let profiling_info = ref Smap.empty

let set_current_query s = current_query := s

let get_current_query () = !current_query

let update_profiling_info query dt =
  let t = Smap.find_opt query !profiling_info |> Option.value ~default:0.0 in
  profiling_info := Smap.add query (t +. dt) !profiling_info

let with_elapsed_time f =
  let t = Sys.time () in
  let v = f () in
  (v, Sys.time () -. t)

let with_current_query ?(profile = false) s f =
  let old = !current_query in
  set_current_query (Some s) ;
  let ret =
    if profile && !profiling_enabled then (
      let ret, dt = with_elapsed_time f in
      update_profiling_info s dt ; ret )
    else f ()
  in
  set_current_query old ; ret

(* For convenience when debugging, we allow storing the model underlying
   the trace being analyzed in a global variable *)
let global_model_ref : Model.t option ref = ref None

let global_model () = Option.get !global_model_ref

let set_global_model m = global_model_ref := Some m

let indent_string ~n s =
  let indent = String.make n ' ' in
  String.concat "\n"
    (List.map (fun line -> indent ^ line) (String.split_on_char '\n' s))

let log ?loc ?details:(ds = []) ?exn ~kind msg =
  let sq s = "[" ^ s ^ "]" in
  let sqopt = function None -> "" | Some s -> sq s in
  let ds =
    match exn with
    | None ->
        ds
    | Some exn ->
        [Printexc.to_string exn; Printexc.get_backtrace ()]
  in
  prerr_endline (sq kind ^ sqopt loc ^ sqopt !current_query ^ ": " ^ msg) ;
  ds |> List.iter (fun d -> prerr_endline (indent_string ~n:4 d))

let info = log ~kind:"Info"

let warn = log ~kind:"Warning"

let error = log ~kind:"Error"

let failwith ?details msg =
  error ?details msg ;
  assert false

(* Useful convenience shortcut for callers *)
let fmt = Format.asprintf

let pp pp_fun x = Format.asprintf "%a" pp_fun x
