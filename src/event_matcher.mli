open Aliases

type related_matchings =
  {link: global_agent_id list; other_constrained: global_agent_id list list}
[@@deriving show, yojson_of]

val match_event : Query.event -> Streaming.window -> related_matchings list
