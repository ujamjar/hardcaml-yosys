(* implement memories as techcells *)

module HardCaml = struct


  let create ~rd ~wr = 
    (* 1 read and 1 write port *)
    assert (List.length rd = 1 && List.length wr = 1);
    let (_,rd), (_,wr) = List.hd rd, List.hd wr in
 
    (* XXX temp the '-> n,...' bit for the printf below *)

    let rdp = Techlib.Simlib.Memrd.P.(map (fun (n,_) -> n,List.assoc n rd.Cell.parameters) t) in
    let rdi = Techlib.Simlib.Memrd.I.(map (fun (n,_) -> n,List.assoc n rd.Cell.inputs) t) in
    let rdo = Techlib.Simlib.Memrd.O.(map (fun (n,_) -> n,List.assoc n rd.Cell.outputs) t) in
    let wrp = Techlib.Simlib.Memwr.P.(map (fun (n,_) -> n,List.assoc n wr.Cell.parameters) t) in
    let wri = Techlib.Simlib.Memwr.I.(map (fun (n,_) -> n,List.assoc n wr.Cell.inputs) t) in
    let wro = Techlib.Simlib.Memwr.O.(map (fun (n,_) -> n,List.assoc n wr.Cell.outputs) t) in

    (* XXX printf what we have -- delete me *)
    let () = 
      Printf.printf "%s\n"
      (String.concat "\n" (List.concat [
        Techlib.Simlib.Memrd.P.(to_list @@ map fst rdp)@
        Techlib.Simlib.Memrd.I.(to_list @@ map fst rdi)@
        Techlib.Simlib.Memrd.O.(to_list @@ map fst rdo)@
        Techlib.Simlib.Memwr.P.(to_list @@ map fst wrp)@
        Techlib.Simlib.Memwr.I.(to_list @@ map fst wri)@
        Techlib.Simlib.Memwr.O.(to_list @@ map fst wro)
      ]))
    in

    ()

end

(* XXX TODO ... *)
module Altera = struct end
module Xilinx = struct end
module Generic = struct end
