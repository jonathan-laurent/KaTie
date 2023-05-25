module Smap = Utils.StringMap

let profiling_enabled = ref false

(* Maps (query, key) to elapsed time *)
let profiling_info : float Smap.t Smap.t ref = ref Smap.empty

let with_elapsed_time f =
  let t = Sys.time () in
  let v = f () in
  (v, Sys.time () -. t)

let update_profiling_info ~query ~key dt =
  let info = !profiling_info in
  let qinfo = Smap.find_opt query info |> Option.value ~default:Smap.empty in
  let t = Smap.find_opt key qinfo |> Option.value ~default:0.0 in
  profiling_info := Smap.add query (Smap.add key (t +. dt) qinfo) info

let record ?query key f =
  if not !profiling_enabled then f ()
  else
    let ret, dt = with_elapsed_time f in
    let query = Option.value query ~default:"" in
    update_profiling_info ~query ~key dt ;
    ret

let yojson_of_query_info info : Yojson.Safe.t =
  `Assoc (info |> Smap.to_list |> List.map (fun (k, t) -> (k, `Float t)))

let total_time qinfo = Smap.fold (fun _ t acc -> t +. acc) qinfo 0.0

let yojson_of_info info : Yojson.Safe.t =
  let compare (_, qi) (_, qi') =
    -Float.compare (total_time qi) (total_time qi')
  in
  `Assoc
    ( info |> Smap.to_list |> List.sort compare
    |> List.map (fun (q, ks) -> (q, yojson_of_query_info ks)) )

let dump_json () = yojson_of_info !profiling_info
