(*****************************************************************************)
(* Utils                                                                     *)
(*****************************************************************************)

open Ppx_yojson_conv_lib.Yojson_conv

module type ShowableOrderedType = sig
  include Map.OrderedType

  val pp : t Fmt.t

  val t_of_yojson : Yojson.Safe.t -> t

  val yojson_of_t : t -> Yojson.Safe.t
end

(* An improved Map module that is augmented with new features. *)
module MapV2 = struct
  module Make (Elt : ShowableOrderedType) = struct
    include Map.Make (Elt)
    open Ppx_yojson_conv_lib.Yojson_conv

    let from_list l = of_seq (List.to_seq l)

    let to_list m = List.of_seq (to_seq m)

    let pp pp_elt = Fmt.iter_bindings iter Fmt.(pair Elt.pp pp_elt)

    let t_of_yojson elt_of_yojson json =
      from_list
        (list_of_yojson (pair_of_yojson Elt.t_of_yojson elt_of_yojson) json)

    let yojson_of_t yojson_of_elt m =
      (yojson_of_list (yojson_of_pair Elt.yojson_of_t yojson_of_elt))
        (to_list m)

    let overriding_update m m' = union (fun _ _x y -> Some y) m m'
  end
end

module IntSet = Set.Make (Int)

module IntMap = MapV2.Make (struct
  type t = int [@@deriving yojson]

  let pp = Fmt.int

  let compare = Int.compare
end)

module StringMap = MapV2.Make (struct
  type t = string [@@deriving yojson]

  let pp = Fmt.string

  let compare = String.compare
end)

let monadic_fold (type acc b) (f : acc -> b -> acc list) (init : acc)
    (l : b list) =
  let f' (accs : acc list) (x : b) : acc list =
    List.concat (List.map (fun acc -> f acc x) accs)
  in
  List.fold_left f' [init] l

let list_of_queue q = List.of_seq (Queue.to_seq q)

let array_of_queue q = Array.of_list (list_of_queue q)

let list_maximum l = List.fold_left max 0 l

let int_of_bool = function true -> 1 | false -> 0

let sum_array = Array.fold_left ( + ) 0

let list_zip l l' = List.map2 (fun x y -> (x, y)) l l'

let list_unzip l = (List.map fst l, List.map snd l)

let no_duplicates cmp l = List.length (List.sort_uniq cmp l) = List.length l

let rec group_list ~compare = function
  | [] ->
      []
  | (k, v) :: rest -> (
    match group_list ~compare rest with
    | [] ->
        [(k, [v])]
    | (k', vs') :: rest' as all' ->
        if compare k k' = 0 then (k, v :: vs') :: rest' else (k, [v]) :: all' )

let sort_and_group_list ~compare l =
  List.sort (fun (k, _) (k', _) -> compare k k') l |> group_list ~compare

let[@tail_mod_cons] rec map_tailrec f = function
  | [] ->
      []
  | x :: xs ->
      f x :: map_tailrec f xs

let partition_tailrec f l =
  let rec aux ts fs = function
    | [] ->
        (List.rev ts, List.rev fs)
    | x :: xs ->
        let ts, fs = if f x then (x :: ts, fs) else (ts, x :: fs) in
        (aux [@tailcall]) ts fs xs
  in
  aux [] [] l
