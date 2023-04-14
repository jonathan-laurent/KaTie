open Aliases

type status = Failure | Success of {other_constrained: global_agent_id list}
[@@deriving show, yojson_of]

type potential_matching = {link: global_agent_id list; status: status}
[@@deriving show, yojson_of]

val match_event : Query.event -> Streaming.window -> potential_matching option
