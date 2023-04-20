type 'a t =
  (* [Cat a b c left right]:
     Elements indexed in the [a,b) interval are in the left child
     Elements indexed in the [b, c) interval are in the right child *)
  | Cat of int * int * int * 'a t * 'a t
  | Leaf of int * 'a (* [Node index object] *)
  | Empty

let rec size = function
  | Cat (_, _, _, l, r) ->
      size l + size r
  | Leaf _ ->
      1
  | Empty ->
      0

let range = function Cat (a, _, c, _, _) -> c - a | Leaf _ -> 1 | Empty -> 0

let init' (a, b) =
  let sep = (a + b) / 2 in
  Cat (a, sep, b, Empty, Empty)

let init size = init' (0, size)

let empty = Empty

type interval = Null | Singleton of int | Interval of int * int

let view_interval (a, b) : interval =
  if b - a <= 0 then Null else if b - a = 1 then Singleton a else Interval (a, b)

let in_interval i (a, b) = i >= a && i < b

(* Widening is cheap so we make the history very small initially *)
let widen_init = 1

(* Ensures that the structure handles range at least [0, i].
   Doubles the struture's capacity if necessary. *)
let rec widen i = function
  | Empty ->
      widen i (init widen_init)
  | Cat (a, _, c, _l, _r) as node ->
      assert (a = 0) ;
      (* Must be called on the top node *)
      if in_interval i (a, c) then node
      else widen i (Cat (0, c, 2 * c, node, Empty))
  | Leaf _ ->
      assert false (* The top node cannot be a leaf node *)

let add i x t =
  let rec add_aux (a, b) i x = function
    | Empty -> (
      match view_interval (a, b) with
      | Null ->
          assert false
      | Singleton i' ->
          assert (i = i') ;
          Leaf (i, x)
      | Interval (a, b) ->
          add_aux (a, b) i x (init' (a, b)) )
    | Leaf (i', _) ->
        assert (i = i') ;
        Leaf (i, x)
    | Cat (a', b', c', lc, rc) ->
        if in_interval i (a', b') then
          Cat (a', b', c', add_aux (a', b') i x lc, rc)
        else if in_interval i (b', c') then
          Cat (a', b', c', lc, add_aux (b', c') i x rc)
        else assert false
  in
  let t = widen i t in
  add_aux (0, range t) i x t

let rec last_before i = function
  | Empty ->
      None
  | Leaf (i', x) ->
      if i' < i then Some (i', x) else None
  | Cat (_a, b, _c, l, r) -> (
      if i <= b then last_before i l
      else
        match last_before i r with Some x -> Some x | None -> last_before i l )

let rec first_after i = function
  | Empty ->
      None
  | Leaf (i', x) ->
      if i' > i then Some (i', x) else None
  | Cat (_a, b, _c, l, r) -> (
      if i >= b - 1 then first_after i r
      else
        match first_after i l with Some x -> Some x | None -> first_after i r )

(* Utility functions *)

let add_list l t = List.fold_right (fun (i, x) acc -> add i x acc) l t

let from_list l =
  if List.exists (fun (i, _) -> i < 0) l then
    raise (Invalid_argument "should only contain nonnegative indices")
  else add_list l empty

let rec to_alist = function
  | Cat (_, _, _, l, r) ->
      to_alist l @ to_alist r
  | Leaf (x, v) ->
      [(x, v)]
  | Empty ->
      []

let pp pp_elt = Fmt.(using to_alist (list (pair int pp_elt)))

let yojson_of_t yojson_of_elt h =
  let open Ppx_yojson_conv_lib.Yojson_conv in
  yojson_of_list (yojson_of_pair yojson_of_int yojson_of_elt) (to_alist h)

(* Some tests *)

let zip_unit l = List.map (fun i -> (i, ())) l

let run_tests () =
  let t = from_list (zip_unit [0; 1; 3; 4; 6; 8]) in
  assert (size t = 6) ;
  assert (first_after 4 t = Some (6, ())) ;
  assert (first_after 3 t = Some (4, ())) ;
  assert (last_before 3 t = Some (1, ())) ;
  assert (last_before 1 t = Some (0, ())) ;
  assert (last_before 0 t = None) ;
  assert (first_after 9 t = None) ;
  print_string "Success."
