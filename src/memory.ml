(* implement memories as techcells *)

open HardCaml.Signal

(* we will do this differently by transforming $memrd/$memwr cells into a $mem cell *)

module HardCaml = struct

  let pint = function `Int i -> i | _ -> failwith "expecting int parameter"
  let pstr = function `String s -> s | _ -> failwith "expecting string parameter"

  module Seq = Make_seq(struct 
    let reg_spec = Seq.r_none 
    let ram_spec = Seq.r_none 
  end)

  (* currently only supports 1 read and 1 write port *)
  let create ~rd ~wr = 
    let open Techlib.Simlib in

    (* 1 read and 1 write port *)
    assert (List.length rd = 1 && List.length wr = 1);
    let (_,rd), (_,wr) = List.hd rd, List.hd wr in
 
    let rdp = Memrd.P.(map (fun (n,_) -> List.assoc n rd.Cell.parameters) t) in
    let wrp = Memwr.P.(map (fun (n,_) -> List.assoc n wr.Cell.parameters) t) in

    (*let rdi = Memrd.I.(map (fun (n,_) -> List.assoc n rd.Cell.inputs) t) in
    let rdo = Memrd.O.(map (fun (n,_) -> List.assoc n rd.Cell.outputs) t) in
    let wri = Memwr.I.(map (fun (n,_) -> List.assoc n wr.Cell.inputs) t) in
    let wro = Memwr.O.(map (fun (n,_) -> List.assoc n wr.Cell.outputs) t) in*)

    assert (pint wrp.Memwr.P.clk_enable = 1); (* async write seems to make no sense *)

    let transparent = pint rdp.Memrd.P.transparent = 1 in
    let async_read = pint rdp.Memrd.P.clk_enable = 0 in
    let width = 
      let wr,ww = pint wrp.Memwr.P.width, pint rdp.Memrd.P.width in
      assert ((wr > 0) && (ww > 0) && (wr = ww));
      wr
    in
    let abits = 
      let ar,aw = pint wrp.Memwr.P.abits, pint rdp.Memrd.P.abits in
      assert ((ar > 0) && (aw > 0) && (ar = aw));
      ar
    in
    let wrpol, rdpol = pint wrp.Memwr.P.clk_polarity = 1, pint rdp.Memrd.P.clk_polarity = 1 in

    (* create the Techlib.cell function *)

    let techlib_cell _ i =
      let rdi = Memrd.I.(map (fun (n,_) -> List.assoc ("RD_"^n) i) t) in
      let wri = Memwr.I.(map (fun (n,_) -> List.assoc ("WR_"^n) i) t) in
      let q = 
        Comb.concat @@ List.rev @@ Array.to_list @@
          Array.init width (fun j ->
            Seq.memory
              ~clk:wri.Memwr.I.clk
              ~clkl:(if wrpol then Comb.vdd else Comb.gnd)
              ~we:Comb.(bit wri.Memwr.I.en j)
              (1 lsl abits)
              ~wa:wri.Memwr.I.addr
              ~d:Comb.(bit wri.Memwr.I.data j)
              ~ra:rdi.Memrd.I.addr)
      in
      [ "DATA", q ]
    in
      
    (* create the replacement Cell.t *)
    let prefix p x = List.map (fun (n,x) -> (p^n),x) x in
    "__generated_memory__", {
      Cell.typ = "__generated_memory__";
      label = "__generated_memory__";
      cell = techlib_cell;
      parameters = [];
      inputs = 
        prefix "RD_" rd.Cell.inputs @ 
        prefix "WR_" wr.Cell.inputs;
      outputs = rd.Cell.outputs;
    }

end

(* XXX TODO ... *)
module Altera = struct end
module Xilinx = struct end
module Generic = struct end
