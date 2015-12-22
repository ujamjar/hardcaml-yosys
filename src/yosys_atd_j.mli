(* Auto-generated from "yosys_atd.atd" *)


type direction = Yosys_atd_t.direction

type bits = Yosys_atd_t.bits

type port = Yosys_atd_t.port = { direction: direction; bits: bits }

type dyn = Yojson.Safe.json

type param_value = Yosys_atd_t.param_value

type attributes = Yosys_atd_t.attributes = { src: string }

type netname = Yosys_atd_t.netname = {
  hide_name: int;
  bits: bits;
  attributes: attributes
}

type cell = Yosys_atd_t.cell = {
  hide_name: int;
  typ: string;
  parameters: (string * param_value) list;
  attributes: attributes;
  port_directions: (string * direction) list;
  connections: (string * bits) list
}

type modl = Yosys_atd_t.modl = {
  ports: (string * port) list;
  cells: (string * cell) list;
  netnames: (string * netname) list
}

type t = Yosys_atd_t.t = { creator: string; modl: (string * modl) list }

val write_direction :
  Bi_outbuf.t -> direction -> unit
  (** Output a JSON value of type {!direction}. *)

val string_of_direction :
  ?len:int -> direction -> string
  (** Serialize a value of type {!direction}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_direction :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> direction
  (** Input JSON data of type {!direction}. *)

val direction_of_string :
  string -> direction
  (** Deserialize JSON data of type {!direction}. *)

val write_bits :
  Bi_outbuf.t -> bits -> unit
  (** Output a JSON value of type {!bits}. *)

val string_of_bits :
  ?len:int -> bits -> string
  (** Serialize a value of type {!bits}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_bits :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> bits
  (** Input JSON data of type {!bits}. *)

val bits_of_string :
  string -> bits
  (** Deserialize JSON data of type {!bits}. *)

val write_port :
  Bi_outbuf.t -> port -> unit
  (** Output a JSON value of type {!port}. *)

val string_of_port :
  ?len:int -> port -> string
  (** Serialize a value of type {!port}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_port :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> port
  (** Input JSON data of type {!port}. *)

val port_of_string :
  string -> port
  (** Deserialize JSON data of type {!port}. *)

val write_dyn :
  Bi_outbuf.t -> dyn -> unit
  (** Output a JSON value of type {!dyn}. *)

val string_of_dyn :
  ?len:int -> dyn -> string
  (** Serialize a value of type {!dyn}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_dyn :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> dyn
  (** Input JSON data of type {!dyn}. *)

val dyn_of_string :
  string -> dyn
  (** Deserialize JSON data of type {!dyn}. *)

val write_param_value :
  Bi_outbuf.t -> param_value -> unit
  (** Output a JSON value of type {!param_value}. *)

val string_of_param_value :
  ?len:int -> param_value -> string
  (** Serialize a value of type {!param_value}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_param_value :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> param_value
  (** Input JSON data of type {!param_value}. *)

val param_value_of_string :
  string -> param_value
  (** Deserialize JSON data of type {!param_value}. *)

val write_attributes :
  Bi_outbuf.t -> attributes -> unit
  (** Output a JSON value of type {!attributes}. *)

val string_of_attributes :
  ?len:int -> attributes -> string
  (** Serialize a value of type {!attributes}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_attributes :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> attributes
  (** Input JSON data of type {!attributes}. *)

val attributes_of_string :
  string -> attributes
  (** Deserialize JSON data of type {!attributes}. *)

val write_netname :
  Bi_outbuf.t -> netname -> unit
  (** Output a JSON value of type {!netname}. *)

val string_of_netname :
  ?len:int -> netname -> string
  (** Serialize a value of type {!netname}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_netname :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> netname
  (** Input JSON data of type {!netname}. *)

val netname_of_string :
  string -> netname
  (** Deserialize JSON data of type {!netname}. *)

val write_cell :
  Bi_outbuf.t -> cell -> unit
  (** Output a JSON value of type {!cell}. *)

val string_of_cell :
  ?len:int -> cell -> string
  (** Serialize a value of type {!cell}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_cell :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> cell
  (** Input JSON data of type {!cell}. *)

val cell_of_string :
  string -> cell
  (** Deserialize JSON data of type {!cell}. *)

val write_modl :
  Bi_outbuf.t -> modl -> unit
  (** Output a JSON value of type {!modl}. *)

val string_of_modl :
  ?len:int -> modl -> string
  (** Serialize a value of type {!modl}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_modl :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> modl
  (** Input JSON data of type {!modl}. *)

val modl_of_string :
  string -> modl
  (** Deserialize JSON data of type {!modl}. *)

val write_t :
  Bi_outbuf.t -> t -> unit
  (** Output a JSON value of type {!t}. *)

val string_of_t :
  ?len:int -> t -> string
  (** Serialize a value of type {!t}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_t :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> t
  (** Input JSON data of type {!t}. *)

val t_of_string :
  string -> t
  (** Deserialize JSON data of type {!t}. *)

