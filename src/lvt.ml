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

module Make(C : Cfg) = struct

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

  let memory ~wr ~rd ~mode = 
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

