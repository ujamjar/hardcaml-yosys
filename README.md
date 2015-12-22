# HardCamlYosys

Use the opensource verilog synthesis tool [Yosys](http://www.clifford.at/yosys)
to read a synthesizable verilog design, convert it to a structural netlist
and save it in a JSON file.

This library can read the JSON netlist file and reconstruct the design in HardCaml.

### Status

HardCaml does not support tri-state buffers in general.  Circuits 
with tri-states will not work.

Certain other featurs such as latches are not fully supported - in some
cases code exists to generate verilog instantiations and this need extending
(for example srlatch).

A few simlib primitives are still not supported in the techlib.  These
will be added as and when test examples are found.

### Example

```
yosys> read_verilog design.v;     # load design
yosys> hierarchy; proc; flatten;  # structural conversion
yosys> write_json design.json     # write json netlist
```

```
#require "hardcaml-yosys";;

(* load json design(s) *)
open HardCamlYosys;;
let designs = Import.load Techlib.Simlib.cells (Io.read (open_in "design.json"));;

(* construct hardcaml circuit *)
open HardCaml
open Signal.Comb
let i,o,fn = List.assoc "mydesign" designs;;
let o = fn (List.map (fun (n,b) -> n, input n b) i);;

(* write back to verilog *)
let circuit = Circuit.make "mydesign" (List.map (fun (n,s) -> output n s) o);;
let () = Rtl.Verilog.write print_string circuit;;
```

