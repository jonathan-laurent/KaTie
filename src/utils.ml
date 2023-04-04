(*****************************************************************************)
(* Utils                                                                     *)
(*****************************************************************************)

module IntSet = Set.Make (Int)

module IntMap = Map.Make (Int)

module StringMap = Map.Make (String)

let monadic_fold (type acc) (type b)
    (f : acc -> b -> acc list)
    (init : acc)
    (l : b list) =
    let f' (accs : acc list) (x : b) : acc list =
        List.concat (List.map (fun acc -> f acc x) accs) in
    List.fold_left f' [init] l

let int_map_from_list l = IntMap.of_seq (List.to_seq l)

let update_int_map m m' = IntMap.union (fun _ _x y -> Some y) m m'

let list_of_queue q = List.of_seq (Queue.to_seq q)

let array_of_queue q = Array.of_list (list_of_queue q)

let list_maximum l = List.fold_left (max) 0 l

let int_of_bool = function true -> 1 | false -> 0

let sum_array = Array.fold_left (+) 0