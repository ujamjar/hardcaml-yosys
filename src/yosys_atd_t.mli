(* Auto-generated from "yosys_atd.atd" *)


type dyn = Yojson.Safe.json

type direction = [ `Input | `Output ]

type bits = dyn list

type port = { direction: direction; bits: bits }

type param_value = dyn

type attributes = { src: string; full_case: int; parallel_case: int }

type netname = { hide_name: int; bits: bits; attributes: attributes }

type cell = {
  hide_name: int;
  typ: string;
  parameters: (string * param_value) list;
  attributes: attributes;
  port_directions: (string * direction) list;
  connections: (string * bits) list
}

type modl = {
  ports: (string * port) list;
  cells: (string * cell) list;
  netnames: (string * netname) list
}

type t = { creator: string; modl: (string * modl) list }
