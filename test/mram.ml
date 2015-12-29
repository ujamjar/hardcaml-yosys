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

module Memory_regs(C : Cfg) : sig
  
  module Wr : interface
    we wa d
  end

  module Rd : interface
    re ra
  end

  val memory_nwr_nrd : wr:t Wr.t array -> rd:t Rd.t array -> fallthrough:bool array -> t array

end = struct

  (* async read memories with multiple read and write ports, implemented as registers *)

  open C

  module Wr = interface
    we[1] wa[abits] d[dbits]
  end

  module Rd = interface
    re[1] ra[abits]
  end

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

module Rand(C : Cfg) = struct
end

(*
module Multiram = struct
  (* XXX can't remember if I tested this properly... *)
  open Comb

  let rec bin2oh s = 
    let (&::) a b = repeat a (width b) &: b in
    if width s = 1 then s @: ~: s 
    else 
        ((((msb s)) &:: bin2oh (lsbs s)) @: 
      ((~: (msb s)) &:: bin2oh (lsbs s)))


  let rec oh2bin s = 
    let pairs s = 
      let s = if width s mod 2 = 0 then s else gnd @: s in
      let b = List.rev (bits s) in
      Utils.zip (Utils.leven b) (Utils.lodd b)
    in
    let enc2_1 (a, b) = (b, a |: b) in
    if width s = 1 then gnd
    else if width s = 2 then bit s 1
    else
      let s, p = Utils.unzip (List.map enc2_1 (pairs s)) in
      oh2bin (concat (List.rev p)) @: reduce (|:) s

  let rec oh2bin_p s = 
    let w = width s in
    let l = Utils.nbits (w-1) in
    let rec f b i = 
      match b with
      | [] -> empty (* shouldnt get here *)
      | h::[] -> consti l i
      | h::t -> mux2 h (consti l i) (f t (i+1))
    in
    f (List.rev (bits s)) 0

  (* lvt multiport ram *)

  type 'a write = 
    {
      we : 'a;
      wd : 'a;
      wa : 'a;
    }
  type 'a read = 
    {
      re : 'a;
      ra : 'a;
    }
  type ram = size:int -> we:t -> wa:t -> d:t -> re:t -> ra:t -> t

  let get_ram = function
    | `rbw -> Seq.ram_rbw
    | `wbr -> Seq.ram_wbr
    | `async -> (fun ~size ~spec ~we ~wa ~d ~re ~ra -> Seq.memory ~size ~spec ~we ~w:wa ~d ~r:ra)

  let ram_1wr ~ram ~size ~wr ~rd = 
    (* 1 write, n read ports *)
    Array.map 
      (fun rd ->
        (get_ram ram) ~size 
          ~we:wr.we ~wa:wr.wa ~d:wr.wd 
          ~re:rd.re ~ra:rd.ra) rd

  let reg_seethru spec we d = 
    mux2 we d (Seq.reg spec we d)

  let lvt ~priority_write ~size ~spec ~wr ~rd = 
    let n_wr, n_rd = Array.length wr, Array.length rd in
    let bin2oh we s = Array.map ((&:) we) (Array.of_list (List.rev (bits (bin2oh s)))) in
    let we1h = Array.map (fun wr -> bin2oh wr.we wr.wa) wr in
    let regs = Array.init size (fun i -> 
      let wes = Array.init n_wr (fun j -> we1h.(j).(i)) in
      let we = reduce (|:) (Array.to_list wes) in
      let oh2bin = if priority_write then oh2bin_p else oh2bin in
      let d = oh2bin (concat (List.rev (Array.to_list wes))) in
      reg_seethru spec we d)
    in
    let regs = Array.to_list regs in
    Array.map (fun rd -> mux rd.ra regs) rd

  let ram ?(priority_write=false) ~ram ~size ~spec ~wr ~rd = 
    let n_wr, n_rd = Array.length wr, Array.length rd in
    let banks = Array.map (fun wr -> ram_1wr ~ram ~size ~wr ~rd) wr in
    let lvt = lvt ~priority_write ~size ~spec ~wr ~rd in
    let lvt = Array.init n_rd (fun i -> Seq.reg spec rd.(i).re lvt.(i)) in

    Array.init n_rd (fun i -> mux lvt.(i) 
      (Array.to_list (Array.init n_wr (fun j -> banks.(j).(i))))) 

end
*)
(*module Seq = Make_seq(struct
  let ram_spec = Seq.r_none
  let reg_spec = Seq.r_none
end)*)

module B = Bits.Comb.IntbitsList
module Cs = Cyclesim.Make(B)
module S = Cyclesim.Api

module Waveterm_waves = HardCamlWaveTerm.Wave.Make(HardCamlWaveTerm.Wave.Bits(B))
module Waveterm_sim = HardCamlWaveTerm.Sim.Make(B)(Waveterm_waves)
module Waveterm_ui = HardCamlWaveLTerm.Ui.Make(B)(Waveterm_waves)

let testbench_mram () = 

  let abits = 4 in
  let dbits = 8 in
  let nrd = 4 in
  let nwr = 4 in
  let rbw = false in

  let mk_rd_port n = 
    let n s = s ^ string_of_int n in
    {
      Multiram.ra = input (n "ra") abits;
      re = input (n "re") 1
    }
  in
  let mk_wr_port n = 
    let n s = s ^ string_of_int n in
    {
      Multiram.wa = input (n "wa") abits;
      we = input (n "we") 1;
      wd = input (n "wd") dbits;
    }
  in
  (*let q = 
    (if rbw then Seq.multi_ram_rbw else Seq.multi_ram_wbr)
      ~rd:(Array.init nrd mk_rd_port)
      ~wr:(Array.init nwr mk_wr_port)
      (1 lsl abits)*)
  let q = 
    Multiram.ram
      ?priority_write:None
      ~ram:((if rbw then Seq.ram_rbw else Seq.ram_wbr) ~spec:Seq.r_none)
      ~size:(1 lsl abits)
      ~spec:Seq.r_none
      ~wr:(Array.init nwr mk_wr_port)
      ~rd:(Array.init nrd mk_rd_port)
  in
  let circ = Circuit.make "mram"
    (Array.to_list @@ Array.mapi (fun j s -> output ("q" ^ string_of_int j) s) q)
  in
  (*let () = Rtl.Verilog.write print_string circ*)
  let sim = Cs.make circ in
  let sim,waves = Waveterm_sim.wrap sim in

  let rd = Array.init nrd (fun j ->
    let n s = s ^ string_of_int j in
    {
      Multiram.ra = S.in_port sim (n "ra");
      re = S.in_port sim (n "re");
    })
  in

  let wr = Array.init nwr (fun j ->
    let n s = s ^ string_of_int j in
    {
      Multiram.wa = S.in_port sim (n "wa");
      we = S.in_port sim (n "we");
      wd = S.in_port sim (n "wd");
    })
  in

  let read (port, addr) = begin
    let open Multiram in
    rd.(port).re := B.vdd;
    rd.(port).ra := B.consti abits addr;
  end in

  let write (port, addr, data) = begin
    let open Multiram in
    wr.(port).we := B.vdd;
    wr.(port).wa := B.consti abits addr;
    wr.(port).wd := B.consti dbits data;
  end in

  let cycle n = begin
    let open Multiram in
    for i=0 to n-1 do
      S.cycle sim;
      for port=0 to nrd-1 do rd.(port).re := B.gnd done;
      for port=0 to nwr-1 do wr.(port).we := B.gnd done
    done
  end in

  let testbench = begin
    Array.iter write [| 0,0,1; 1,1,3; 2,2,7; 3,3,15 |];
    Array.iter read [| 0,0; 1,1; 2,2; 3,3 |];
    cycle 1;
    Array.iter write [| 0,1,9 |];
    Array.iter read [| 0,0; 1,1; 2,2; 3,3 |];
    cycle 1;
    Array.iter read [| 0,0; 1,1; 2,2; 3,3 |];
    cycle 3;
  end in

  Lwt_main.run (Waveterm_ui.run Waveterm_waves.({ cfg=default; waves })) 

let testbench_mem_regs () = 

  let module C = struct
    let abits = 4 
    let dbits = 8 
    let size = 16
    let spec = Seq.r_none
  end in
  let nwr = 2 in
  let nrd = 2 in
  let fallthrough = [| false; true |] in

  let module M = Memory_regs(C) in
  let aname m n = n ^ string_of_int m in
  let mk_input m (n,b) = input (aname m n) b in
  let q = M.memory_nwr_nrd
    ~wr:(Array.init nwr (fun m -> M.Wr.(map (mk_input m) t)))
    ~rd:(Array.init nrd (fun m -> M.Rd.(map (mk_input m) t)))
    ~fallthrough
  in
  let qi = Array.init nrd (fun m -> input (aname m "qi") C.dbits) in
  let qs = Array.init nrd (fun m -> 
    output (aname m "q") q.(m), 
    output (aname m "qr") qi.(m)) 
  in
  let check = Array.mapi (fun m (q,qr) -> output (aname m "check") (q ==: qr)) qs in
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
  let rand_rd () = M.Rd.(map rand t) in

  (* implement the reads and writes *)
  let ref_mem = Array.init C.size (fun _ -> 0) in
 
  (* do random reads and writes to the core and a testbench memory *)
  let assign a (b,c) = a := B.consti b c in

  let update_write wr = 
    let wr = M.Wr.(map snd wr) in
    if wr.M.Wr.we = 1 then 
      ref_mem.(wr.M.Wr.wa) <- wr.M.Wr.d
  in
  let update_read i rd = 
    let rd = M.Rd.({ map snd rd with M.Rd.re = 1 }) in
    if rd.M.Rd.re = 1 then 
      qi.(i) := B.consti C.dbits ref_mem.(rd.M.Rd.ra)
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

  for i=0 to 1000 do
    perform_reads false; (* non-fallthrough reads *)
    perform_writes (); (* all writes (last takes priority) *)
    perform_reads true; (* fallthrough reads *)
    S.cycle sim;
  done;

  Lwt_main.run (Waveterm_ui.run Waveterm_waves.({ cfg=default; waves })) 


let () = testbench_mem_regs ()

