type t = 
  {
    typ : string;
    label : string;
    cell : Techlib.cell;
    parameters : (string * Yosys_atd_t.param_value) list;
    inputs : (string * Yosys_atd_t.bits) list;
    outputs : (string * Yosys_atd_t.bits) list;
  }
