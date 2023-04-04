(* Sparse persistent dictionnaries indexed by integers.
   This structure is optimized to answer requests of the kind
   "what is the last element in t whose index is strictly greater than i" *)

type 'a t

val init : int -> 'a t

val empty : 'a t

val size : 'a t -> int

val range : 'a t -> int

val add : int -> 'a -> 'a t -> 'a t

val add_list : (int * 'a) list -> 'a t -> 'a t

val last_before : int -> 'a t -> (int * 'a) option

val first_after : int -> 'a t -> (int * 'a) option

val run_tests : unit -> unit
