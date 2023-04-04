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
