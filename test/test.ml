#directory "_build/src";;
#require "atdgen,hardcaml";;
#load "HardCamlYosys.cma";;

open HardCaml.Signal.Comb
open HardCamlYosys

let load name = 

  let circ = Io.read (open_in name) in
  let fns = Import.load Techlib.Simlib.cells circ in
  List.iter 
    (fun (n,(i,_,f)) ->
      let inputs = List.map (fun (n,b) -> n, input n b) i in
      let outputs = f inputs in
      let circ = HardCaml.Circuit.make n (List.map (fun (n,s) -> output n s) outputs) in
      HardCaml.Rtl.Verilog.write print_string circ)
  fns 

let () = load "./yosys/yosys-github/tests/json/adder.json"
let () = load "./yosys/yosys-github/tests/json/out2.json"

