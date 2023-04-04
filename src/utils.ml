(*****************************************************************************)
(* Utils                                                                     *)
(*****************************************************************************)

open Ppx_yojson_conv_lib.Yojson_conv
module IntSet = Set.Make (Int)
module IntMap = Map.Make (Int)
module StringMap = Map.Make (String)

type 'a int_map = 'a IntMap.t

let int_map_from_list l = IntMap.of_seq (List.to_seq l)

let pp_int_map pp_elt = Fmt.(iter_bindings IntMap.iter (pair int pp_elt))

let int_map_of_yojson elt_of_yojson json =
  int_map_from_list
    (list_of_yojson (pair_of_yojson int_of_yojson elt_of_yojson) json)

let yojson_of_int_map yojson_of_elt m =
  (yojson_of_list (yojson_of_pair yojson_of_int yojson_of_elt))
    (List.of_seq (IntMap.to_seq m))

let monadic_fold (type acc b) (f : acc -> b -> acc list) (init : acc)
    (l : b list) =
  let f' (accs : acc list) (x : b) : acc list =
    List.concat (List.map (fun acc -> f acc x) accs)
  in
  List.fold_left f' [init] l

let update_int_map m m' = IntMap.union (fun _ _x y -> Some y) m m'

let list_of_queue q = List.of_seq (Queue.to_seq q)

let array_of_queue q = Array.of_list (list_of_queue q)

let list_maximum l = List.fold_left max 0 l

let int_of_bool = function true -> 1 | false -> 0

let sum_array = Array.fold_left ( + ) 0
