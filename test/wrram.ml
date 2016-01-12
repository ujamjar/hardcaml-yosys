open HardCaml
open Signal
open Comb
open HardCamlYosys

module B = Bits.Comb.IntbitsList
module Cs = Cyclesim.Make(B)
module S = Cyclesim.Api

module Waveterm_waves = HardCamlWaveTerm.Wave.Make(HardCamlWaveTerm.Wave.Bits(B))
module Waveterm_sim = HardCamlWaveTerm.Sim.Make(B)(Waveterm_waves)
module Waveterm_ui = HardCamlWaveLTerm.Ui.Make(B)(Waveterm_waves)

let testbench () = 

  let module C = struct
    let abits = 4
    let dbits = 8
    let size = (1 lsl abits)
  end in

  let module L = Lvt.Make_wren(C) in
  let nwr = 2 in
  let nrd = 1 in
  let layout = [|
    [| 0;0;1;1;2;2;3;3 |];
    [| 4;4;4;4;5;5;5;5 |];
  |] in
  let mode = Array.init nrd (fun _ -> `sync_rbw) in
  let aname m n = n ^ string_of_int m in
  let mk_input m (n,b) = input (aname m n) b in
  let wr = Array.init nwr (fun m -> L.Wr.(map (mk_input m) t)) in
  let rd = Array.init nrd (fun m -> L.Rd.(map (mk_input m) t)) in
  let wr' = Array.init nwr (fun m -> { Lvt.wspec=Seq.r_none; wr=wr.(m) }) in
  let rd' = Array.init nrd (fun m -> { Lvt.rspec=Seq.r_none; rd=rd.(m); mode=mode.(m) }) in
  let q = L.memory ~layout ~wr:wr' ~rd:rd' in
  let q = Array.init nrd (fun m -> output (aname m "q") q.(m)) in
  let circ = Circuit.make "wrram" (Array.to_list q) in
  let sim = Cs.make circ in
  let sim,waves = Waveterm_sim.wrap sim in

  let rd m = 
    {
      L.Rd.ra = S.in_port sim (aname m "ra");
      re = try S.in_port sim (aname m "re") with _ -> ref B.vdd;
    }
  in

  let wr m = 
    {
      L.Wr.wa = S.in_port sim (aname m "wa");
      we = S.in_port sim (aname m "we");
      d = S.in_port sim (aname m "d");
    }
  in

  let rd, wr = Array.init nrd rd, Array.init nwr wr in
 
  let open L.Wr in
  let open L.Rd in
  let cycle() = 
    S.cycle sim;
    Array.iter (fun wr -> wr.we := B.consti C.dbits 0) wr;
    Array.iter (fun rd -> rd.re := B.gnd) rd;
  in

  wr.(0).wa := B.consti C.abits 0;
  wr.(0).we := B.const "00000011";
  wr.(0).d := B.consti C.dbits 255;
  wr.(1).wa := B.consti C.abits 1;
  wr.(1).we := B.const "11110000";
  wr.(1).d := B.consti C.dbits 255;
  cycle ();
  rd.(0).ra := B.consti C.abits 0;
  rd.(0).re := B.vdd;
  cycle ();
  rd.(0).ra := B.consti C.abits 1;
  rd.(0).re := B.vdd;
  cycle ();
  cycle ();
  cycle ();

  Lwt_main.run (Waveterm_ui.run Waveterm_waves.({ cfg=default; waves }))

let () = testbench ()

