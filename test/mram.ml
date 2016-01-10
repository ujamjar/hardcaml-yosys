(* 
 
  Multiport memories for HardCaml
  ===============================

  The basic memory primitive provide by HardCaml has one write (wr) and
  one read (rd) port.  The write is synchronous and the read is asynchronous.
  By adding a register to either the read address or output data we can
  generate standard read-before-write and write-before-read synchronous
  memories.  The read and write clocks can be from different domains.

  A simple way to build a multi-port memory is out of simple registers
  This is, of course, very inefficient.  One restriction that
  still exists is that each write port must be from the same clock 
  domain.  I (think) the reads ports could potentially be in differing
  clock domains.

  LVT
  ===

  To do a bit better we can instead build multi-port memories using
  a Live Value Table (LVT).  The idea here is in 3 steps;

  1] To make N rd ports, we replicate a 1 rd ram N times each written
     with the same data.  Note each replicated RAM will contain exactly
     the same data.  The replication provides N access ports to this data.
     Call this memory_nrd.

  2] To make M wr ports we initially replicate memory_nrd M times.
     Each write port is connected to one memory_nrd instance.  Note that
     each memory_nrd bank will this time contain different data.
     We now have M * N ram outputs to select between to for the N read
     ports.

  3] Build the live value table.  This tracks the bank which was most 
     recently written for each address in the RAM.  This is built as
     a multi-port memory itself, specifically using the simple register
     scheme described before.  The outputs of the LVT (one for each read
     port) selects the bank built in step 2 that contains the most 
     recently written value for a particular address.

  Consider 2 read, 4 write ports on a 256 element x 32 bit memory.  
  Using the pure register scheme we will need to generate 256x32 
  register bits, write port selection logic at the input to each 
  register, and 2 256x32 muxes to select the read data.

  With the LVT scheme we still need a register based multiport
  memory - in this case 2 read, 4 write ports on a 256 element x 2 bit
  memory.  Note the change here - we went from a 32 bit memory (to
  store the data) to a 2 bit memory (to store the index of the latest
  write bank ie it's of width log2(number of write ports).  In this
  case we have save approx 16x logic resources.

  Of course we also need 2*4 256 x 32 memories (with 1 read and 1 write
  port each) to store the data.


*)
open HardCaml
open Signal
open Comb

module type Cfg = sig
  val abits : int
  val dbits : int
  val size : int
end

module Wr = interface
  wclk we wa d
end

module Rd = interface
  rclk re ra
end

(* fallthrough = wbr (write before read) *)
type mode = [ `async_rbw | `async_wbr | `sync_rbw | `sync_wbr ]

let is_sync = function `sync_wbr | `sync_rbw -> true | _ -> false
let is_async m = not (is_sync m)
let is_rbw = function `sync_rbw | `async_rbw -> true | _ -> false
let is_wbr m = not (is_rbw m)

module Ports(C : Cfg) = struct
  module Wr = struct
    include Wr 
    let bits = { wclk = 1; we = 1; wa = C.abits; d = C.dbits }
    let t = map2 (fun (n,_) b -> n,b) t bits
  end
  module Rd = struct
    include Rd 
    let bits = { rclk = 1; re = 1; ra = C.abits }
    let t = map2 (fun (n,_) b -> n,b) t bits
  end
end

module Multiport_regs(C : Cfg) : sig
  
  include module type of Ports(C)

  val memory : wr:t Wr.t array -> rd:t Rd.t array -> mode:mode array -> t array

end = struct

  (* async read memories with multiple read and write ports, implemented as registers *)

  open C

  include Ports(C)

  open Wr
  open Rd

  let pri = tree 2 (function [a] -> a 
                           | [s0,d0;s1,d1] -> s1 |: s0, mux2 s1 d1 d0
                           | _ -> empty,empty)

  let spec clk = { Seq.r_none with Types.reg_clock = clk }

  let reg_we_enable ~we ~wa = 
    select (binary_to_onehot wa) (size-1) 0 &: mux2 we (ones size) (zero size)

  let memory_nwr_array ~wr = 
    let wclk = wr.(0).wclk in (* note; we can only support one write clock! *)
    let wr = Array.to_list wr in
    let we1h = List.map (fun wr -> reg_we_enable ~we:wr.we ~wa:wr.wa) wr in
    Array.to_list @@ Array.init size 
      (fun elt ->
        let wed = List.map2 (fun we1h wr -> we1h.[elt:elt], wr.d) we1h wr in
        let we,d = pri wed in (* last d with write enable set *)
        let r = Seq.reg (spec wclk) we d in
        we, d, r)

  (* n write, n read ports *)
  let memory ~wr ~rd ~mode = 
    let base = memory_nwr_array ~wr in
    Array.init (Array.length rd) 
      (fun i -> 
        let reg = Seq.reg (spec rd.(i).rclk) in
        let mr = List.map (fun (we,d,r) -> mux2 we d r) base in
        let r = List.map (fun (_,_,r) -> r) base in
        match mode.(i) with
        | `async_wbr -> mux rd.(i).ra mr
        | `async_rbw -> mux rd.(i).ra r
        | `sync_wbr -> mux (reg rd.(i).re rd.(i).ra) r
        | `sync_rbw -> reg rd.(i).re (mux rd.(i).ra r))

end

module Lvt(C : Cfg) = struct

  include Ports(C)
  open Wr
  open Rd

  let spec clk = { Seq.r_none with Types.reg_clock = clk }

  let memory_1rd ~wr ~rd ~mode = 
    let wspec, rspec = spec wr.wclk, spec rd.rclk in
    match mode with
    | `sync_rbw -> Seq.reg rspec rd.re (Seq.memory C.size wspec wr.we wr.wa wr.d rd.ra)
    | `sync_wbr -> Seq.memory C.size wspec wr.we wr.wa wr.d (Seq.reg rspec rd.re rd.ra)
    | `async_rbw -> Seq.memory C.size wspec wr.we wr.wa wr.d rd.ra
    | `async_wbr -> 
      mux2 (wr.we &: (wr.wa ==: rd.ra)) wr.d (Seq.memory C.size wspec wr.we wr.wa wr.d rd.ra)

  let memory_nrd ~wr ~rd ~mode = 
    let nrd = Array.length rd in
    Array.init nrd (fun i -> memory_1rd ~wr ~rd:rd.(i) ~mode:mode.(i))

  let memory_nwr_nrd ~wr ~rd ~mode = 
    let nwr, nrd = Array.length wr, Array.length rd in
    assert (Array.length mode = nrd);

    (* create the live value table *)
    let module Lvt_cfg = struct
      let abits = C.abits
      let dbits = Utils.clog2 nwr
      let size = C.size
    end in
    let module Lvt = Multiport_regs(Lvt_cfg) in
    let lvt = 
      if nwr = 1 then [||]
      else
        let lvt_wr = Array.init nwr (fun i -> { wr.(i) with d = consti Lvt_cfg.dbits i; }) in
        Lvt.memory ~wr:lvt_wr ~rd ~mode 
    in

    (* create the memory banks *)
    let mem = Array.init nwr (fun i -> memory_nrd ~wr:wr.(i) ~rd ~mode) in

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
let random_re = ref true
let mode : [ `alternate | mode ] ref = ref `alternate
let vlog = ref false

let _ = Arg.(parse [
  "-n", Set_int(n_cycles), "Number of cycles to simulate (default: 1000)";
  "-a", Set_int(abits), "address bits (default: 4)";
  "-d", Set_int(dbits), "data bits (default: 8)";
  "-rd", Set_int(nrd), "read ports (default: 2)";
  "-wr", Set_int(nwr), "write ports (default: 2)";
  "-no-re", Clear(random_re), "disable random read-enable toggling (default: false)";
  "-sync-rbw", Unit(fun () -> mode := `sync_rbw), "Put all ports in sync_rbw mode";
  "-sync-wbr", Unit(fun () -> mode := `sync_wbr), "Put all ports in sync_wbr mode";
  "-async-rbw", Unit(fun () -> mode := `async_rbw), "Put all ports in async_rbw mode";
  "-async-wbr", Unit(fun () -> mode := `async_wbr), "Put all ports in async_wbr mode";
  "-vlog", Set(vlog), "Dump verilog";
] (fun _ -> failwith "invalid anon arg")
"LVT Multiport Memory Testbench.

Builds memories with N read/M write ports from simpler 1 read/1 write 
port memories (as available in FPGAs).  Requires N*M base memories 
plus a so called Live Value Table to directs reads to the most
recently accessed write data.

Each port may be set independently in syncronous or asynchronous read mode with
read-before-write or write-before-read behaviour (by default the testbench alternates 
between port modes).

For testing the read-enable port may be held constant.
")

let testbench_mem_regs () = 

  let module C = struct
    let abits = !abits
    let dbits = !dbits
    let size = (1 lsl abits)
  end in
  let nwr = !nwr in
  let nrd = !nrd in
  let mode = Array.init nrd (fun i ->
    match !mode with
    | `alternate -> 
        (match i mod 4 with 0 -> `sync_rbw | 1 -> `sync_wbr | 2 -> `async_rbw | _ -> `async_wbr)
    | `sync_rbw -> `sync_rbw
    | `sync_wbr -> `sync_wbr
    | `async_rbw -> `async_rbw
    | `async_wbr -> `async_wbr) 
  in
  let random_re = !random_re in

  (*let module M = Memory_regs(C) in*)
  let module L = Lvt(C) in
  let aname m n = n ^ string_of_int m in
  let mk_input m (n,b) = input (aname m n) b in
  let wr = Array.init nwr (fun m -> L.Wr.(map (mk_input m) t)) in
  let rd = Array.init nrd (fun m -> L.Rd.(map (mk_input m) t)) in
  let q = L.memory_nwr_nrd ~wr ~rd ~mode in
  let qi = Array.init nrd (fun m -> input (aname m "qi") C.dbits) in
  let reg_qr m = 
    match mode.(m) with
    | `sync_wbr -> Seq.reg Seq.r_none vdd qi.(m)
    | `sync_rbw -> Seq.reg Seq.r_none rd.(m).L.Rd.re qi.(m)
    | _ -> qi.(m)
  in
  let qr = Array.init nrd reg_qr in
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
      L.Rd.rclk = ref B.gnd;
      ra = S.in_port sim (aname m "ra");
      re = try S.in_port sim (aname m "re") with _ -> ref B.vdd;
    }
  in

  let wr m = 
    {
      L.Wr.wclk = ref B.gnd;
      wa = S.in_port sim (aname m "wa");
      we = S.in_port sim (aname m "we");
      d = S.in_port sim (aname m "d");
    }
  in

  let rd, wr = Array.init nrd rd, Array.init nwr wr in

  let qi = Array.init nrd (fun m -> S.in_port sim (aname m "qi")) in

  (* random reads and writes *)
  let rand (_,b) = b, Random.int (1 lsl b) in
  let rand_wr () = L.Wr.(map rand t) in
  let rand_rd () = 
    if random_re then L.Rd.(map rand t) 
    else
      L.Rd.{ map rand t with re = 1, 1 } 
  in

  (* implement the reads and writes *)
  let ref_mem = Array.init C.size (fun _ -> 0) in
 
  (* do random reads and writes to the core and a testbench memory *)
  let assign a (b,c) = a := B.consti b c in

  let update_write wr = 
    let wr = L.Wr.(map snd wr) in
    if wr.L.Wr.we = 1 then 
      ref_mem.(wr.L.Wr.wa) <- wr.L.Wr.d
  in
  let prev_ra = Array.make nrd 0 in
  let update_read i rd = 
    (*let rd = L.Rd.({ map snd rd with L.Rd.re = 1 }) in*)
    let rd = L.Rd.(map snd rd) in
    if rd.L.Rd.re = 1 || is_async mode.(i) then begin
      qi.(i) := B.consti C.dbits ref_mem.(rd.L.Rd.ra);
      prev_ra.(i) <- rd.L.Rd.ra
    end else if mode.(i) = `sync_wbr then begin
      qi.(i) := B.consti C.dbits ref_mem.(prev_ra.(i))
    end
  in

  let perform_reads ft = 
    let cond = function `sync_wbr | `async_wbr -> ft
                      | _ -> not ft
    in
    for i=0 to nrd-1 do
      if cond mode.(i) then begin
        let rd' = rand_rd() in
        update_read i rd';
        ignore @@ L.Rd.(map2 assign rd.(i) rd')
      end
    done;
  in

  let perform_writes () = 
    for i=0 to nwr-1 do
      let wr' = rand_wr() in
      update_write wr';
      ignore @@ L.Wr.(map2 assign wr.(i) wr')
    done;
  in

  for i=1 to !n_cycles do
    perform_reads false; (* non-fallthrough reads *)
    perform_writes (); (* all writes (last takes priority) *)
    perform_reads true; (* fallthrough reads *)
    S.cycle sim;
  done;

  Lwt_main.run (Waveterm_ui.run Waveterm_waves.({ cfg=default; waves }));
  (if !vlog then Rtl.Verilog.write print_string circ)


let () = testbench_mem_regs ()

