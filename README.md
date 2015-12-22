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
will be added as and when test examples are found.  Most importantly
native memories are not supprted yet (though there is a workaround).

### Example

#### Yosys usage

```
yosys> read_verilog design.v;     # load design
yosys> hierarchy; proc; flatten;  # structural conversion
yosys> write_json design.json     # write json netlist
```

In designs with multiple modules you may require `hierarchy -top <top_module_name>`.

The `memory` pass is useful to convert memories to basic cells (registers and address
decoders).

#### HardCaml usage

```ocaml
#require "hardcaml-yosys";;

let convert json_file core_name = 

  (* load json design(s) *)
  let open HardCamlYosys in
  let designs = Import.load Techlib.Simlib.cells (Io.read (open_in json_file)) in

  (* construct hardcaml circuit *)
  let open HardCaml in
  let open Signal.Comb in
  let i,o,fn = List.assoc core_name designs in
  let o = fn (List.map (fun (n,b) -> n, input n b) i) in

  (* write back to verilog *)
  let circuit = Circuit.make "mydesign" (List.map (fun (n,s) -> output n s) o) in
  Rtl.Verilog.write print_string circuit 

let () = convert "design.json" "mytop"
```

