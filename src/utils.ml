(*****************************************************************************)
(* Utils                                                                     *)
(*****************************************************************************)

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
