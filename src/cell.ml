module Y = Yosys_atd_t 

type bits = int list

type t = 
  {
    typ : string;
    label : string;
    parameters : (string * Y.param_value) list;
    inputs : (string * bits) list;
    outputs : (string * bits) list;
  }

exception No_cell_direction_specified of string * string
exception Invalid_net_tristate
exception Invalid_net_bit_specifier of string
exception Unsupported_parameter_type of string * string

let net_of_bit = function
  | `Int i -> i
  | `String "x" | `String "X" -> 0
  | `String "z" | `String "Z" -> raise Invalid_net_tristate
  | `String x -> (try int_of_string x with _ -> raise (Invalid_net_bit_specifier x))
  | _ -> raise (Invalid_net_bit_specifier "unknown json type")

let mk_params cell_name parameters = 
  List.map (function
    | name,`Int i -> name, HardCaml.Signal.Types.ParamInt i
    | name, `String s -> name, HardCaml.Signal.Types.ParamString s
    | name,_ -> raise (Unsupported_parameter_type(cell_name,name))) parameters

let partition_ios cell = 
  List.partition (fun (n,b) ->
    try List.assoc n cell.Y.port_directions = `Input
    with Not_found -> raise (No_cell_direction_specified(cell.Y.typ, n)))
  cell.Y.connections

let mk_cell cell = 
  let inputs, outputs = partition_ios cell in
  let port (name, bits) = name, List.map net_of_bit bits in
  let inputs, outputs = List.map port inputs, List.map port outputs in
  {
    typ = cell.Y.typ;
    label = Y.(cell.attributes.src);
    parameters = cell.Y.parameters;
    inputs; outputs;
  }

