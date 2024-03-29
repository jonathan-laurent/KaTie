(*****************************************************************************)
(* KaTie's Custom Logging Module                                             *)
(*****************************************************************************)

let current_query = ref None

let set_current_query s = current_query := s

let get_current_query () = !current_query

let with_current_query s f =
  let old = !current_query in
  set_current_query (Some s) ;
  let ret = f () in
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
