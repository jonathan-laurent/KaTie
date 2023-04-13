open Aliases

type status = Failure | Success of {other_constrained: global_agent_id list}
[@@deriving show, yojson_of]

type result = No_match | Match of {index: global_agent_id list; status: status}
[@@deriving show, yojson_of]

val match_event : Query.event -> Streaming.window -> result
