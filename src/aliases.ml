open Ppx_yojson_conv_lib.Yojson_conv

type pat_agent_id = int [@@deriving show, yojson]

type local_agent_id = int [@@deriving show, yojson]

type global_agent_id = int [@@deriving show, yojson]

type local_event_id = int [@@deriving show, yojson]

type global_event_id = int [@@deriving show, yojson]

type local_comp_id = int [@@deriving show, yojson]

type measure_id = int [@@deriving show, yojson]

type agent_kind = int [@@deriving show, yojson]

type site_id = int [@@deriving show, yojson]

type int_state = int [@@deriving show, yojson]

type pat_site = pat_agent_id * site_id [@@deriving show, yojson]

type local_site = local_agent_id * site_id [@@deriving show, yojson]

type global_site = global_agent_id * site_id [@@deriving show, yojson]

type 'a local_agent_id_map = 'a Utils.IntMap.t [@@deriving show, yojson_of]

type 'a pat_agent_id_map = 'a Utils.IntMap.t [@@deriving show, yojson_of]
