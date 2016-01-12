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
  we wa d
end

module Rd = interface
  re ra
end

(* fallthrough = wbr (write before read) *)
type mode = [ `async_rbw | `async_wbr | `sync_rbw | `sync_wbr ]

let is_sync = function `sync_wbr | `sync_rbw -> true | _ -> false
let is_async m = not (is_sync m)
let is_rbw = function `sync_rbw | `async_rbw -> true | _ -> false
let is_wbr m = not (is_rbw m)

type wr_port = 
  {
    wr : t Wr.t;
    wspec : Signal.Types.register;
  }

type rd_port = 
  {
    rd : t Rd.t;
    rspec : Signal.Types.register;
    mode : mode;
  }

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

module Multiport_regs(C : Cfg) = struct

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
    let wspec = wr.(0).wspec in
    let wr = List.map (fun wr -> wr.wr) @@ Array.to_list wr in
    let we1h = List.map (fun wr -> reg_we_enable ~we:wr.we ~wa:wr.wa) wr in
    Array.to_list @@ Array.init size 
      (fun elt ->
        let wed = List.map2 (fun we1h wr -> we1h.[elt:elt], wr.d) we1h wr in
        let we,d = pri wed in (* last d with write enable set *)
        let r = Seq.reg wspec we d in
        we, d, r)

  (* n write, n read ports *)
  let memory ~wr ~rd = 
    let base = memory_nwr_array ~wr in
    Array.init (Array.length rd) 
      (fun i -> 
        let reg = Seq.reg rd.(i).rspec in
        let mr = List.map (fun (we,d,r) -> mux2 we d r) base in
        let r = List.map (fun (_,_,r) -> r) base in
        match rd.(i).mode with
        | `async_wbr -> mux rd.(i).rd.ra mr
        | `async_rbw -> mux rd.(i).rd.ra r
        | `sync_wbr -> mux (reg rd.(i).rd.re rd.(i).rd.ra) r
        | `sync_rbw -> reg rd.(i).rd.re (mux rd.(i).rd.ra r))

end

module Make(C : Cfg) = struct

  include Ports(C)
  open Wr
  open Rd

  let spec clk = { Seq.r_none with Types.reg_clock = clk }

  let memory_1rd ~wr ~rd = 
    let wspec, rspec, mode = wr.wspec, rd.rspec, rd.mode in
    let wr, rd = wr.wr, rd.rd in
    match mode with
    | `sync_rbw -> Seq.reg rspec rd.re (Seq.memory C.size wspec wr.we wr.wa wr.d rd.ra)
    | `sync_wbr -> Seq.memory C.size wspec wr.we wr.wa wr.d (Seq.reg rspec rd.re rd.ra)
    | `async_rbw -> Seq.memory C.size wspec wr.we wr.wa wr.d rd.ra
    | `async_wbr -> 
      mux2 (wr.we &: (wr.wa ==: rd.ra)) wr.d (Seq.memory C.size wspec wr.we wr.wa wr.d rd.ra)

  let memory_nrd ~wr ~rd = 
    let nrd = Array.length rd in
    Array.init nrd (fun i -> memory_1rd ~wr ~rd:rd.(i))

  let memory ~wr ~rd = 
    let nwr, nrd = Array.length wr, Array.length rd in

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
        let lvt_wr = Array.init nwr (fun i -> 
          { wr.(i) with wr = { wr.(i).wr with d = consti Lvt_cfg.dbits i } }) in
        Lvt.memory ~wr:lvt_wr ~rd 
    in

    (* create the memory banks *)
    let mem = Array.init nwr (fun i -> memory_nrd ~wr:wr.(i) ~rd) in

    (* select the correct memory bank *)
    Array.init nrd 
      (fun rd ->
        let mem = Array.init nwr (fun wr -> mem.(wr).(rd)) in
        if nwr = 1 then mem.(0) 
        else mux lvt.(rd) (Array.to_list mem))

end

module Make_wren(C : Cfg) = struct

  module Wr = struct
    include Wr 
    let bits = { we = C.dbits; wa = C.abits; d = C.dbits }
    let t = map2 (fun (n,_) b -> n,b) t bits
  end
  module Rd = struct
    include Rd 
    let bits = { re = 1; ra = C.abits }
    let t = map2 (fun (n,_) b -> n,b) t bits
  end

  module L = Make(C)

  let get_layout wrnets =
    let runs list = 
      let rec f acc prev l = 
        match prev, l with
        | None, [] -> []
        | None, h::t -> f acc (Some(h,1)) t
        | Some(prev, run), [] -> List.rev ((prev,run) :: acc)
        | Some(prev, run), h::t ->
            if prev = h then f acc (Some(prev, run+1)) t
            else f ((prev, run) :: acc) (Some(h,1)) t
      in
      f [] None list
    in
    let transpose a = 
      let i0 = Array.length a in
      let i1 = Array.length a.(0) in
      Array.init i1 (fun i1 -> Array.init i0 (fun i0 -> a.(i0).(i1)))
    in
    let rec starts pos = function
      | [] -> []
      | (h,r)::t -> (h,pos,r) :: starts (pos+r) t
    in
    starts 0 @@ runs @@ Array.to_list @@ transpose wrnets 
  
  let memory ~layout = 
    let layout = get_layout layout in
    let memory ~wr ~rd = 
      let nrd = Array.length rd in
      let concat l =
        Array.init nrd (fun i -> concat @@ List.rev @@ List.map (fun x -> x.(i)) l)
      in
      concat @@ List.map 
        (fun (_,n,bits) ->
          let sel_wr wr = 
            { wr with 
              wr = { wr.wr with
                Wr.we = select wr.wr.Wr.we n n;
                d = select wr.wr.Wr.d (n+bits-1) n;
              }
            }
          in
          let wr = Array.map sel_wr wr in
          L.memory ~wr ~rd)
      layout
    in
    memory

end

