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
  | VNull (* Only for internal use *)
  | VBool of bool
  | VInt of int
  | VFloat of float
  | VString of string
  | VAgentSet of AgentSet.t
[@@deriving show, yojson_of]

type _ value_type =
  | TNull : unit value_type
  | TBool : bool value_type
  | TInt : int value_type
  | TFloat : float value_type
  | TString : string value_type
  | TAgentSet : AgentSet.t value_type

type any_value_type = T : 'a value_type -> any_value_type

let typeof = function
  | VNull ->
      T TNull
  | VBool _ ->
      T TBool
  | VInt _ ->
      T TInt
  | VFloat _ ->
      T TFloat
  | VString _ ->
      T TString
  | VAgentSet _ ->
      T TAgentSet

let value_type_to_string = function
  | T TNull ->
      "null"
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

let to_string = function
  | VNull ->
      "null"
  | VBool b ->
      Int.to_string (Utils.int_of_bool b)
  | VInt x ->
      Int.to_string x
  | VFloat x ->
      Fmt.str "%.17g" x
  | VString s ->
      Fmt.str "\"%s\"" s
  | VAgentSet _ ->
      "<agents>"

let int_of_bool = function true -> 1 | false -> 0

let cast : type a. a value_type -> t -> a option =
 fun ty v ->
  match (ty, v) with
  | TNull, VNull ->
      Some ()
  | TBool, VBool b ->
      Some b
  | TInt, VInt n ->
      Some n
  | TInt, VBool b ->
      Some (int_of_bool b)
  | TFloat, VFloat x ->
      Some x
  | TFloat, VInt x ->
      Some (float_of_int x)
  | TString, VString s ->
      Some s
  | TAgentSet, VAgentSet ags ->
      Some ags
  | _, _ ->
      None

let equal v v' =
  match (v, v') with
  | VNull, VNull ->
      Some true
  | VNull, _ | _, VNull ->
      Some false
  | VBool b, VBool b' ->
      Some (Bool.equal b b')
  | VInt n, VInt n' ->
      Some (Int.equal n n')
  | VFloat x, VFloat x' ->
      Some (Float.equal x x')
  | VInt n, VFloat x | VFloat x, VInt n ->
      Some (Float.equal x (float_of_int n))
  | VString s, VString s' ->
      Some (String.equal s s')
  | VAgentSet s, VAgentSet s' ->
      Some (AgentSet.equal s s')
  | _, _ ->
      None
