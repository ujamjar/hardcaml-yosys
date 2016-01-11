exception Cell_not_in_techlib of string * string
exception No_cell_direction_specified of string * string
exception Unsupported_parameter_type of string * string
exception Failed_to_find_net of int
exception Input_not_found of string
exception Empty_bus
exception Invalid_net_tristate
exception Invalid_net_bit_specifier of string

module I : Map.S
module S : Map.S

type create_fn = HardCaml.Signal.Comb.t Techlib.assoc -> HardCaml.Signal.Comb.t Techlib.assoc

val modl_wire_map : 
  Yosys_atd_t.port Techlib.assoc -> Cell.t Techlib.assoc ->
  HardCaml.Signal.Comb.t S.t *
  HardCaml.Signal.Comb.t S.t list *
  (int * HardCaml.Signal.Comb.t) I.t

val black_box_of_cell : Yosys_atd_t.cell -> Techlib.cell

val load : 
  (* allow black boxes *)
  ?blackbox:bool ->
  (* techlib *)
  string list * Techlib.cell Techlib.assoc -> 
  (* design(s) *)
  Yosys_atd_t.t -> 
  (* construction *)
  (int Techlib.assoc * int Techlib.assoc * create_fn) Techlib.assoc

