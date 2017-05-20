open HardCaml
open Printf

exception Cell_not_in_techlib of string * string
exception Failed_to_find_net of int
exception Input_not_found of string
exception Empty_bus of string
exception Expecting_memory_id
module I = Map.Make(struct type t = int let compare = compare end)
module S = Map.Make(struct type t = string let compare = compare end)
module N = Map.Make(struct type t = int list let compare = compare end)
module Y = Yosys_atd_t 

type create_fn = HardCaml.Signal.Comb.t Techlib.assoc -> HardCaml.Signal.Comb.t Techlib.assoc

let get_net_name_map netnames = 
  let add map (name,netname) = 
    let bits = List.map Cell.net_of_bit netname.Y.bits in
    if N.mem bits map then 
      let n = N.find bits map in
      N.add bits (name::n) map
    else
      N.add bits [name] map
  in
  List.fold_left add N.empty netnames 

(* something drives a net; it's either a module input, or a cell output. 

    create wires and a map from bits (nets) to wires/indexes for module 
    inputs and cell outputs *)
let modl_wire_map ~busnames ~ports ~cells = 
  let map = List.fold_left (fun map (b,s) -> I.add b (0,s) map) I.empty 
    [ 0, (Signal.Comb.gnd); 1, (Signal.Comb.vdd) ]
  in

  (* generate a wire, and create a mapping from the net index to the wire and offset *)
  let wire_with_name busnames bits_ = 
    let open Signal.Comb in
    let wire = wire (List.length bits_) in
    try 
      let names = N.find bits_ busnames in
      wire, names
    with Not_found -> 
      wire, []
  in
  let map_of_bits map (name,bits_) = 
    let wire,bname = wire_with_name busnames bits_ in
    (name,wire,bname), snd @@ List.fold_left 
      (fun (idx,map) bit ->
        (* note; adding [bit] if it is already in the list 
            should be an error (well...except for trisates!) *)
        (*printf "%i -> %Li[%i]\n" bit (Signal.Types.uid wire) idx;*)
        idx+1, I.add bit (idx,wire) map) (0,map) bits_
  in

  let map_with_list f map x = 
    let ws,map = List.fold_left (fun (ws,map) x -> 
      let w,map = f map x in
      w::ws, map) ([],map) x
    in
    List.rev ws,map
  in

  let map_with_smap f map x = 
    let ws,map = map_with_list f map x in
    List.fold_right (fun (n,s,m) map -> S.add n (s,m) map) ws S.empty, map
  in

  (* mapping to module inputs *)
  let mi, map = 
    map_with_smap
      (fun map (n,(port:Y.port)) -> map_of_bits map (n, List.map Cell.net_of_bit port.Y.bits))
      map @@ List.filter (fun (_,port) -> port.Y.direction = `Input) ports
  in

  (* mapping to cell outputs *)
  let co, map = 
    map_with_list 
      (fun map (_,x,_) -> map_with_smap map_of_bits map x.Cell.outputs) 
      map cells 
  in

  mi, co, map

let celltyp (_,cell) = cell.Cell.typ 
let memid (_,cell) = 
  match List.assoc "MEMID" cell.Cell.parameters with
  | `String s -> s
  | _ -> raise Expecting_memory_id
  | exception _ -> raise Expecting_memory_id

(*let is_memrd cell = celltyp cell = "$memrd" 
let is_memwr cell = celltyp cell = "$memwr" 
let is_mem cell = is_memrd cell || is_memwr cell 

(* extract memrd/memwr cells and sort by their ids XXX meminit *)
let get_memory_cells cells = 
  (* sort memories by id *)
  let module M = Map.Make(String) in
  let map = 
    List.fold_left 
      (fun map cell ->
        if not (is_mem cell) then map
        else
          let memid = memid cell in
          try 
            let (r,w) = M.find memid map in
            M.add memid (if is_memrd cell then ((cell::r),w) else (r,(cell::w))) map
          with _ ->
            M.add memid (if is_memrd cell then ([cell],[]) else ([],[cell])) map)
      M.empty cells
  in
  M.bindings map
  
(* replace memrd/memwr with new techlbs cells implementing the appropriate
   memory for each memid *)
let convert_memories cells = 
  let mems = get_memory_cells cells in
  let mem_cells = List.map (fun (memid, (rd,wr)) -> Memory.HardCaml.create ~rd ~wr) mems in
  let cells = List.filter (fun cell -> not (is_mem cell)) cells in
  mem_cells @ cells
*)

(* create black box for cell *)
let black_box_of_cell inst_name cell  = 
  let _, outputs = Cell.partition_ios cell in
  let f _ p i = 
    let inst = Signal.Instantiation.inst ~instance:inst_name cell.Y.typ p i
      (List.map (fun (n,b) -> n, List.length b) outputs)
    in
    List.map (fun (n,_) -> n, inst#o n) outputs
  in
  f

let get_bus cell_name map bus = 
  let open Signal.Comb in
  let find b =
    try I.find b map 
    with Not_found -> raise (Failed_to_find_net(b))
  in
  let simple = false in
  if simple then
    concat @@ List.rev @@ List.map (fun b -> let i,w = find b in bit w i) bus
  else (* consolidate bus, where possible *)
    let rec opt (w,l,h) bus = 
      match bus with
      | [] -> [(w,l,h)]
      | b::bus ->
        let open Signal.Types in
        let i,w' = find b in
        if i=(h+1) && (uid w') = (uid w) then opt (w,l,h+1) bus
        else (w,l,h) :: opt (w',i,i) bus
    in
    match bus with
    | [] -> raise (Empty_bus cell_name)
    | h::t -> 
      let i,w = find h in
      let l = opt (w,i,i) t in
      concat @@ List.rev @@ List.map (fun (w,l,h) -> select w h l) l

let get_bus_of_nets name map bus = get_bus name map (List.map Cell.net_of_bit bus) 

let load_modl ~blackbox ~keepnames (black_boxes,techlib) (name,modl) = 
  let bbmap = List.fold_left (fun map m -> S.add ("$"^m) m map) S.empty black_boxes in

  (* find cell in the techlib *)
  let find_cell inst_name cell = 
    (* match the cell in the techlib *)
    match List.assoc cell.Y.typ techlib with
    | cell -> cell
    | exception Not_found -> begin
      (* could be a blackbox cell from the techlib *)
      match S.find cell.Y.typ bbmap with
      | n -> black_box_of_cell inst_name { cell with Y.typ = n }
      (* otherwise pure blackbox, if allowed *)
      | exception Not_found when blackbox -> black_box_of_cell inst_name cell
      | exception Not_found -> raise Y.(Cell_not_in_techlib(cell.typ, cell.attributes.src))
    end
  in

  (* get cell with explicit inputs and outputs and map to techlib *)
  let mk_cell (inst_name,cell) = 
    let cell' = Cell.mk_cell cell in
    inst_name, cell', ((find_cell inst_name cell) cell') 
  in

  (* instantiate all the cells *)
  let open Signal.Comb in

  let instantiate_cell map (cell_name,cell,cell_fn) co = 
    (* create parameters *)
    let params = Cell.mk_params cell_name cell.Cell.parameters in
    (* create input busses *)
    let inputs = List.map (fun (name,bits) -> name, get_bus (cell_name ^ ": " ^ name) map bits) cell.Cell.inputs in 
    (* instantiate module *)
    let outputs = cell_fn params inputs in 
    (* connect outputs *)
    let (--) a b = List.fold_left (--) a b in
    List.iter (fun (n,o) -> let w,m = S.find n co in w <== (o -- m)) outputs
  in

  let instantiate map cells co = List.iter2 (instantiate_cell map) cells co in

  (* connect together wires *)
  let connect_inputs inputs mi = 
    let find n = 
      try fst (S.find n mi)
      with Not_found -> raise (Input_not_found(n))
    in
    List.iter (fun (n,i) -> find n <== i) inputs
  in

  let collect_outputs map outputs = 
    List.map (fun (n,(port:Y.port)) -> n, get_bus_of_nets ("output: "^n) map port.Y.bits) outputs
  in

  let load_modl modl =
    let cells = List.map mk_cell modl.Y.cells in
    let busnames = if keepnames then get_net_name_map modl.Y.netnames else N.empty in
    (*let cells = convert_memories cells in*)
    let mi,co,map = modl_wire_map ~busnames ~ports:modl.Y.ports ~cells:cells in
    instantiate map cells co;
    mi,co,map
  in

  let is_input (n, (p : Y.port)) = p.Y.direction = `Input in
  let is_output (n, (p : Y.port)) = p.Y.direction = `Output in
  let inputs, outputs = List.partition is_input modl.Y.ports in
  let port (n,(p : Y.port)) = n, List.length p.Y.bits in
  let inputs, outputs = List.map port inputs, List.map port outputs in

  let create i = 
    let mi, co, map = load_modl modl in
    let outputs = List.filter is_output modl.Y.ports in
    connect_inputs i mi;
    collect_outputs map outputs
  in
  name, (inputs, outputs, create)

let load ?(blackbox=true) ?(keepnames=false) techlib t = 
  List.map (load_modl ~blackbox ~keepnames techlib) t.Y.modl


