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
  | TBool : bool value_type
  | TInt : int value_type
  | TFloat : float value_type
  | TString : string value_type
  | TAgentSet : AgentSet.t value_type
  | TTuple : t list value_type

type any_value_type = T : 'a value_type -> any_value_type

let value_type = function
  | Bool _ ->
      T TBool
  | Int _ ->
      T TInt
  | Float _ ->
      T TFloat
  | String _ ->
      T TString
  | Agent_set _ ->
      T TAgentSet
  | Tuple _ ->
      T TTuple

let value_type_to_string = function
  | T TBool ->
      "bool"
  | T TInt ->
      "int"
  | T TFloat ->
      "float"
  | T TString ->
      "string"
  | T TAgentSet ->
      "agents"
  | T TTuple ->
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
  | TBool, Bool b ->
      Either.right b
  | TInt, Int n ->
      Either.right n
  | TFloat, Float x ->
      Either.right x
  | TString, String s ->
      Either.right s
  | TAgentSet, Agent_set ags ->
      Either.right ags
  | TTuple, Tuple t ->
      Either.right t
  | _, _ ->
      let expected = value_type_to_string (T ty) in
      let got = value_type_to_string (value_type v) in
      Either.left {expected; got}
