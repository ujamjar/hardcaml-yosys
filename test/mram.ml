(* put together a test for the multiram component which we want to use in the 
 * memory generator logic (with some possible extensions) *)

open HardCaml
open Signal
open Comb

module type Cfg = sig
  val abits : int
  val dbits : int
  val size : int
  val spec : Types.register
end

module Wr = interface
  we wa d
end

module Rd = interface
  re ra
end

module Ports(C : Cfg) = struct
  module Wr = struct
    include Wr 
    let bits = { we = 1; wa = C.abits; d = C.dbits }
    let t = map2 (fun (n,_) b -> n,b) t bits
  end
  module Rd = struct
    include Rd 
    let bits = { re = 1; ra = C.abits }
    let t = map2 (fun (n,_) b -> n,b) t bits
  end
end

module Memory_regs(C : Cfg) : sig
  
  include module type of Ports(C)

  val memory_nwr_nrd : wr:t Wr.t array -> rd:t Rd.t array -> fallthrough:bool array -> t array

end = struct

  (* async read memories with multiple read and write ports, implemented as registers *)

  open C

  include Ports(C)

  open Wr
  open Rd

  let pri = tree 2 (function [a] -> a 
                           | [s0,d0;s1,d1] -> s1 |: s0, mux2 s1 d1 d0
                           | _ -> empty,empty)

  let reg_we_enable ~we ~wa = 
    select (binary_to_onehot wa) (size-1) 0 &: mux2 we (ones size) (zero size)

  let memory_nwr_array ~wr = 
    let wr = Array.to_list wr in
    let we1h = List.map (fun wr -> reg_we_enable ~we:wr.we ~wa:wr.wa) wr in
    Array.to_list @@ Array.init size 
      (fun elt ->
        let wed = List.map2 (fun we1h wr -> we1h.[elt:elt], wr.d) we1h wr in
        let we,d = pri wed in (* last d with write enable set *)
        let r = Seq.reg spec we d in
        we, d, r)

  (* n write, n read ports *)
  let memory_nwr_nrd ~wr ~rd ~fallthrough = 
    let base = memory_nwr_array ~wr in
    Array.init (Array.length rd) 
      (fun i -> 
        let base = List.map 
          (fun (we,d,r) -> if fallthrough.(i) then mux2 we d r else r) 
          base
        in
        mux rd.(i).ra base) 

end

module Lvt(C : Cfg) = struct

  include Ports(C)
  open Wr
  open Rd

  let memory_nrd ~wr ~rd ~fallthrough = 
    let nrd = Array.length rd in
    Array.init nrd (fun i ->
      let ram = if fallthrough.(i) then Seq.ram_wbr else Seq.ram_rbw in 
      let rd = rd.(i) in
      ram ~spec:C.spec ~size:C.size
        ~we:wr.we ~wa:wr.wa ~d:wr.d 
        ~re:rd.re ~ra:rd.ra) 

  let memory_nwr_nrd ~wr ~rd ~fallthrough = 
    let nwr, nrd = Array.length wr, Array.length rd in
    assert (Array.length fallthrough = nrd);

    (* create the live value table *)
    let module Lvt_cfg = struct
      let abits = C.abits
      let dbits = Utils.clog2 nwr
      let size = C.size
      let spec = C.spec
    end in
    let module Lvt = Memory_regs(Lvt_cfg) in
    let lvt = 
      if nwr = 1 then [||]
      else
        let lvt_wr = Array.init nwr (fun i -> { wr.(i) with d = consti Lvt_cfg.dbits i; }) in
        let lvt_rd = Array.init nrd (fun i -> 
          { rd.(i) with ra = 
              if fallthrough.(i) then Seq.reg C.spec rd.(i).re rd.(i).ra else rd.(i).ra 
          })
        in
        let lvt = Lvt.memory_nwr_nrd ~wr:lvt_wr ~rd:lvt_rd ~fallthrough:(Array.make nrd false) in
        Array.init nrd (fun i -> 
          if fallthrough.(i) then lvt.(i)
          else Seq.reg C.spec rd.(i).re lvt.(i)) 
    in

    (* create the memory banks *)
    let mem = Array.init nwr (fun i -> memory_nrd ~wr:wr.(i) ~rd ~fallthrough) in

    (* select the correct memory bank *)
    Array.init nrd 
      (fun rd ->
        let mem = Array.init nwr (fun wr -> mem.(wr).(rd)) in
        if nwr = 1 then mem.(0) 
        else mux lvt.(rd) (Array.to_list mem))

end

module B = Bits.Comb.IntbitsList
module Cs = Cyclesim.Make(B)
module S = Cyclesim.Api

module Waveterm_waves = HardCamlWaveTerm.Wave.Make(HardCamlWaveTerm.Wave.Bits(B))
module Waveterm_sim = HardCamlWaveTerm.Sim.Make(B)(Waveterm_waves)
module Waveterm_ui = HardCamlWaveLTerm.Ui.Make(B)(Waveterm_waves)

(* configuration *)

let n_cycles = ref 1000
let abits = ref 4
let dbits = ref 8
let nwr = ref 2
let nrd = ref 2
let lvt = ref true
let random_re = ref true
let all_fall = ref false
let none_fall = ref false

let _ = Arg.(parse [
  "-n", Set_int(n_cycles), "Number of cycles to simulate (default: 1000)";
  "-a", Set_int(abits), "address bits (default: 4)";
  "-d", Set_int(dbits), "data bits (default: 8)";
  "-rd", Set_int(nrd), "read ports (default: 2)";
  "-wr", Set_int(nwr), "write ports (default: 2)";
  (*"-regs", Clear(lvt), "build async register based memory (default: lvt)";*)
  "-no-re", Clear(random_re), "disable random read-enable toggling (default: false)";
  "-fallthrough", Set(all_fall), "Put all ports in fallthrough mode (default: alternate)";
  "-no-fallthrough", Set(none_fall), "Put all ports in non-fallthrough mode (default: alternate)";
] (fun _ -> failwith "invalid anon arg")
"LVT Multiport Memory Testbench.

Builds memories with N read/M write ports from simpler 1 read/1 write 
port memories (as available in FPGAs).  Requires N*M base memories 
plus a so called Live Value Table to directs reads to the most
recently accessed write data.

Each port may be set independantly in read-before-write or 
write-before-read mode (by default the testbench alternates port 
modes).

For testing the read-enable port may be held constant.
")

let testbench_mem_regs () = 

  let module C = struct
    let abits = !abits
    let dbits = !dbits
    let size = (1 lsl abits)
    let spec = Seq.r_none
  end in
  let nwr = !nwr in
  let nrd = !nrd in
  let fallthrough = Array.init nrd (fun i -> 
    if !all_fall then true
    else if !none_fall then false
    else i mod 2 = 1) in
  let lvt = !lvt in
  let random_re = !random_re in

  let module M = Memory_regs(C) in
  let module L = Lvt(C) in
  let aname m n = n ^ string_of_int m in
  let mk_input m (n,b) = input (aname m n) b in
  let wr = Array.init nwr (fun m -> M.Wr.(map (mk_input m) t)) in
  let rd = Array.init nrd (fun m -> M.Rd.(map (mk_input m) t)) in
  let q = (if lvt then L.memory_nwr_nrd else M.memory_nwr_nrd) ~wr ~rd ~fallthrough in
  let qi = Array.init nrd (fun m -> input (aname m "qi") C.dbits) in
  let reg_qr m = Seq.reg C.spec (if fallthrough.(m) then vdd else rd.(m).M.Rd.re) qi.(m) in
  let qr = Array.init nrd (if lvt then reg_qr else (fun m -> qi.(m))) in
  let qs = Array.init nrd (fun m -> 
    output (aname m "q") q.(m), 
    output (aname m "qr") qr.(m))
  in
  let check = Array.map (fun (q,qr) -> (q ==: qr)) qs in
  let check = Array.mapi (fun m -> output (aname m "check")) check in
  let outputs = 
    (Array.to_list @@ Array.map fst qs) @
    (Array.to_list @@ Array.map snd qs) @
    (Array.to_list check)
  in
  let circ = Circuit.make "memreg" outputs in
  let sim = Cs.make circ in
  let sim,waves = Waveterm_sim.wrap sim in
 
  let rd m = 
    {
      M.Rd.ra = S.in_port sim (aname m "ra");
      re = try S.in_port sim (aname m "re") with _ -> ref B.vdd;
    }
  in

  let wr m = 
    {
      M.Wr.wa = S.in_port sim (aname m "wa");
      we = S.in_port sim (aname m "we");
      d = S.in_port sim (aname m "d");
    }
  in

  let rd, wr = Array.init nrd rd, Array.init nwr wr in

  let qi = Array.init nrd (fun m -> S.in_port sim (aname m "qi")) in

  (* random reads and writes *)
  let rand (_,b) = b, Random.int (1 lsl b) in
  let rand_wr () = M.Wr.(map rand t) in
  let rand_rd () = 
    if random_re then M.Rd.(map rand t) 
    else
      M.Rd.{ map rand t with re = 1, 1 } 
  in

  (* implement the reads and writes *)
  let ref_mem = Array.init C.size (fun _ -> 0) in
 
  (* do random reads and writes to the core and a testbench memory *)
  let assign a (b,c) = a := B.consti b c in

  let update_write wr = 
    let wr = M.Wr.(map snd wr) in
    if wr.M.Wr.we = 1 then 
      ref_mem.(wr.M.Wr.wa) <- wr.M.Wr.d
  in
  let prev_ra = Array.make nrd 0 in
  let update_read i rd = 
    (*let rd = M.Rd.({ map snd rd with M.Rd.re = 1 }) in*)
    let rd = M.Rd.(map snd rd) in
    if rd.M.Rd.re = 1 then begin
      qi.(i) := B.consti C.dbits ref_mem.(rd.M.Rd.ra);
      prev_ra.(i) <- rd.M.Rd.ra
    end else if fallthrough.(i) then begin
      qi.(i) := B.consti C.dbits ref_mem.(prev_ra.(i))
    end
  in

  let perform_reads ft = 
    for i=0 to nrd-1 do
      if fallthrough.(i) = ft then begin
        let rd' = rand_rd() in
        update_read i rd';
        ignore @@ M.Rd.(map2 assign rd.(i) rd')
      end
    done;
  in

  let perform_writes () = 
    for i=0 to nwr-1 do
      let wr' = rand_wr() in
      update_write wr';
      ignore @@ M.Wr.(map2 assign wr.(i) wr')
    done;
  in

  for i=1 to !n_cycles do
    perform_reads false; (* non-fallthrough reads *)
    perform_writes (); (* all writes (last takes priority) *)
    perform_reads true; (* fallthrough reads *)
    S.cycle sim;
  done;

  Lwt_main.run (Waveterm_ui.run Waveterm_waves.({ cfg=default; waves })) 


let () = testbench_mem_regs ()

