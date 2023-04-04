type agent_id = int [@@deriving show, yojson]

type event_id = int [@@deriving show, yojson]

type measure_id = int [@@deriving show, yojson]

type agent_kind = int [@@deriving show, yojson]

type site_id = int [@@deriving show, yojson]

type int_state = int [@@deriving show, yojson]

type site = agent_id * site_id [@@deriving show, yojson]
