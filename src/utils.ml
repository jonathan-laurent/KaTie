(*****************************************************************************)
(* Utils                                                                     *)
(*****************************************************************************)

module Int = 
struct
    type t = int
    let compare = compare
end

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

let map_option f = function
    | Some x -> Some (f x)
    | None -> None

let bind_option mx f = 
    match mx with
    | Some x -> f x
    | None -> None

let int_map_from_list l = 
    List.fold_left (fun acc (k, v) ->
        IntMap.add k v acc
    ) IntMap.empty l

let update_int_map m m' = 
    IntMap.union (fun _ _x y -> Some y) m m'

(* /!\ It is important to reverse the order of the list *)
let list_of_queue q = List.rev (Queue.fold (fun acc x -> x :: acc) [] q)

let array_of_queue q = Array.of_list (list_of_queue q)

let rec map_list_option f = function
  | [] -> []
  | x :: xs -> 
    begin match f x with
      | None -> map_list_option f xs
      | Some fx -> fx :: map_list_option f xs
    end

let concat_map f l = List.concat (List.map f l)

let default d = function
    |  Some x -> x
    | _ -> d

let list_maximum l = List.fold_left (max) 0 l

let int_of_bool = function
    | true -> 1
    | false -> 0