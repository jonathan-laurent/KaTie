type pat_agent_id = int [@@deriving show, yojson]

type local_agent_id = int [@@deriving show, yojson]

type global_agent_id = int [@@deriving show, yojson]

type local_event_id = int [@@deriving show, yojson]

type global_event_id = int [@@deriving show, yojson]

type measure_id = int [@@deriving show, yojson]

type agent_kind = int [@@deriving show, yojson]

type site_id = int [@@deriving show, yojson]

type int_state = int [@@deriving show, yojson]

type pat_site = pat_agent_id * site_id [@@deriving show, yojson]

type local_site = local_agent_id * site_id [@@deriving show, yojson]

type global_site = global_agent_id * site_id [@@deriving show, yojson]
