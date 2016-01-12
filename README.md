# HardCamlYosys

Use the opensource verilog synthesis tool [Yosys](http://www.clifford.at/yosys)
to read a synthesizable verilog design, convert it to a structural netlist
and save it in a JSON file.

This library can read the JSON netlist file and reconstruct the design in HardCaml.

### Status

HardCaml does not support tri-state buffers in general.  Circuits 
with tri-states will not work.

A few simlib primitives are not supported in the techlib.  These
either wont work in HardCaml (ie latches) or have yet to be implemented.
In these cases a blackbox module is generated (the implementation of 
which can be taken from the yosys simlib).

|Status              | Modules | 
|--------------------|---------|
| to do              | sr, shiftx, fsm, macc, alu, mem |
| bbox only          | dlatch, dlatchsr |
| no support planned | tribuf, div, mod, pow, memwr, memrd, meminit, assert, assume, equiv |

### Memories

Yosys can represent memories in a variety of ways

1. Synthesized into technology primitives (ie Xilinx block RAM)  `Supported by black boxes`
2. Converted to registers and muxes `fully supported`
3. As a $mem cell `supported with some limitations`
4. As a combination of $memwr, $memrd and $meminit cells `not supported`

The 2nd option is quite general and should be usable in most cases.  That said the
netlist will now implement all memories as registers so the design - as HardCaml sees it -
may not be very efficient.  Uses the following command in yosys.

```
yosys> memory -dff
```

The third option will attempt to keep memories, but implement them using HardCaml
memory primitives.  HardCaml only supports memories with one read and one
write port whereas in general we need to support multi-port memories with 

* N read ports
* M write ports
* Each write port may be in a different clock domain
* Each read port may be in a different clock domain
* Each read port may be synchronous or asynchronous
* Each read port may be read-before-write or write-before-read (also called fallthrough).

To support yosys we use a construction called a [LVT multi-port memory](http://fpgacpu.ca/multiport)
which builds more general memory structures from simpler single port memories.  The following
limitations are known

1. only supports 1 write clock domain
2. read-before-write and write-before-read behaviour only really makes sense if the read and
   write clocks are in the same clock domain.

In yosys use;

```
yosys> memory -nomap; opt; clean
```

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
  let circuit = Circuit.make core_name (List.map (fun (n,s) -> output n s) o) in
  Rtl.Verilog.write print_string circuit 

let () = convert "design.json" "mytop"
```

