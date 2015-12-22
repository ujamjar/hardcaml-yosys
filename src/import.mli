exception Cell_not_in_techlib of string * string
exception No_cell_direction_specified of string * string
exception Unsupported_parameter_type of string * string
exception Failed_to_find_net of int
exception Input_not_found of string
exception Empty_bus

module I : Map.S
module S : Map.S

type cell
type create_fn = HardCaml.Signal.Comb.t Techlib.assoc -> HardCaml.Signal.Comb.t Techlib.assoc

val modl_wire_map : 
  Yosys_atd_t.port Techlib.assoc -> cell Techlib.assoc ->
  HardCaml.Signal.Comb.t S.t *
  HardCaml.Signal.Comb.t S.t list *
  (int * HardCaml.Signal.Comb.t) I.t

val load : Techlib.cell Techlib.assoc -> Yosys_atd_t.t -> 
  (int Techlib.assoc * int Techlib.assoc * create_fn) Techlib.assoc
