(*****************************************************************************)
(* Dynamically typed values                                                  *)
(*****************************************************************************)

(* The 'component' measure in particular returns sets of agents. *)
module AgentSet = struct
  include Agent.SetMap.Set

  let agent_to_string = Fmt.to_to_string (Agent.print ~with_id:true)

  let pp = Fmt.(using elements (list (Agent.print ~with_id:true)))

  let yojson_of_t m =
    yojson_of_list (fun a -> yojson_of_string (agent_to_string a)) (elements m)
end

type t =
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | Agent_set of AgentSet.t
  | Tuple of t list
[@@deriving show, yojson_of]

type _ value_type =
  | Ty_Bool : bool value_type
  | Ty_Int : int value_type
  | Ty_Float : float value_type
  | Ty_String : string value_type
  | Ty_Agent_set : AgentSet.t value_type
  | Ty_Tuple : t list value_type

type packed_value_type = Ty : 'a value_type -> packed_value_type

let value_type = function
  | Bool _ ->
      Ty Ty_Bool
  | Int _ ->
      Ty Ty_Int
  | Float _ ->
      Ty Ty_Float
  | String _ ->
      Ty Ty_String
  | Agent_set _ ->
      Ty Ty_Agent_set
  | Tuple _ ->
      Ty Ty_Tuple

let value_type_to_string = function
  | Ty Ty_Bool ->
      "bool"
  | Ty Ty_Int ->
      "int"
  | Ty Ty_Float ->
      "float"
  | Ty Ty_String ->
      "string"
  | Ty Ty_Agent_set ->
      "agents"
  | Ty Ty_Tuple ->
      "tuple"

let rec to_string = function
  | Bool b ->
      Int.to_string (Utils.int_of_bool b)
  | Int x ->
      Int.to_string x
  | Float x ->
      Fmt.str "%.17g" x
  | String s ->
      s
  | Agent_set _ ->
      "<agents>"
  | Tuple xs ->
      String.concat ", " (List.map to_string xs)

type cast_error = {expected: string; got: string}

let cast : type a. a value_type -> t -> (cast_error, a) Either.t =
 fun ty v ->
  match (ty, v) with
  | Ty_Bool, Bool b ->
      Either.right b
  | Ty_Int, Int n ->
      Either.right n
  | Ty_Float, Float x ->
      Either.right x
  | Ty_String, String s ->
      Either.right s
  | Ty_Agent_set, Agent_set ags ->
      Either.right ags
  | Ty_Tuple, Tuple t ->
      Either.right t
  | _, _ ->
      let expected = value_type_to_string (Ty ty) in
      let got = value_type_to_string (value_type v) in
      Either.left {expected; got}
