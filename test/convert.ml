(* convert a module from a json file *)

let json = ref ""
let typ = ref "verilog"
let core = ref ""
let bbox = ref false

let () = 
  Arg.parse [
    "-i", Arg.Set_string(json), "input json file";
    "-c", Arg.Set_string(core), "core name";
    "-t", Arg.String(function
      | "verilog" | "vlog" -> typ:="verilog"
      | "vhdl" -> typ:="vhdl"
      | _ -> failwith "invalid type"), "output file type (verilog or vhdl)";
    "-b", Arg.Set(bbox), "allow generation of black boxes";
  ] (fun _ -> failwith "anon args not allowed") "yosys json convert"


open HardCaml.Signal.Comb
open HardCamlYosys

let load name = 

  let circ = Io.read (open_in name) in
  let fns = Import.load ~blackbox:!bbox Techlib.Simlib.cells circ in
  let (n,(i,_,f)) =
    if !core="" then List.hd fns
    else !core, List.assoc !core fns 
  in
  let inputs = List.map (fun (n,b) -> n, input n b) i in
  let outputs = f inputs in
  let circ = HardCaml.Circuit.make n (List.map (fun (n,s) -> output n s) outputs) in
  if !typ = "verilog" then
    HardCaml.Rtl.Verilog.write print_string circ
  else
    HardCaml.Rtl.Vhdl.write print_string circ

let () = 
  if !json="" then failwith "no json file specified"
  else load !json

