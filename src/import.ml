open HardCaml
open Printf

exception Cell_not_in_techlib of string * string
exception No_cell_direction_specified of string * string
exception Unsupported_parameter_type of string * string
exception Failed_to_find_net of int
exception Input_not_found of string
exception Empty_bus
exception Invalid_net_tristate
exception Invalid_net_bit_specifier of string
exception Expecting_memory_id
module I = Map.Make(struct type t = int let compare = compare end)
module S = Map.Make(struct type t = string let compare = compare end)
module Y = Yosys_atd_t 

type create_fn = HardCaml.Signal.Comb.t Techlib.assoc -> HardCaml.Signal.Comb.t Techlib.assoc

let net_of_bit = function
  | `Int i -> i
  | `String "x" | `String "X" -> 0
  | `String "z" | `String "Z" -> raise Invalid_net_tristate
  | `String x -> (try int_of_string x with _ -> raise (Invalid_net_bit_specifier x))
  | _ -> raise (Invalid_net_bit_specifier "unknown json type")

(* something drives a net; it's either a module input, or a cell output. 

    create wires and a map from bits (nets) to wires/indexes for module 
    inputs and cell outputs *)
let modl_wire_map ports cells = 
  let map = List.fold_left (fun map (b,s) -> I.add b (0,s) map) I.empty 
    [ 0,Signal.Comb.gnd; 1,Signal.Comb.vdd ]
  in

  (* generate a wire, and create a mapping from the net index to the wire and offet *)
  let map_of_bits map (name,bits) = 
    let wire = Signal.Comb.wire (List.length bits) in
    (name,wire), snd @@ List.fold_left 
      (fun (idx,map) bit ->
        let bit = net_of_bit bit in
        (* note; adding [bit] if it is already in the list 
            should be an error (well...except for trisates!) *)
        (*printf "%i -> %Li[%i]\n" bit (Signal.Types.uid wire) idx;*)
        idx+1, I.add bit (idx,wire) map) (0,map) bits
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
    List.fold_right (fun (n,s) map -> S.add n s map) ws S.empty, map
  in

  (* mapping to module inputs *)
  let mi, map = 
    map_with_smap
      (fun map (n,(port:Y.port)) -> map_of_bits map (n,port.Y.bits))
      map @@ List.filter (fun (_,port) -> port.Y.direction = `Input) ports
  in

  (* mapping to cell outputs *)
  let co, map = 
    map_with_list 
      (fun map (_,x) -> map_with_smap map_of_bits map x.Cell.outputs) 
      map cells 
  in

  mi, co, map

let celltyp (_,cell) = cell.Cell.typ 
let memid (_,cell) = 
  match List.assoc "MEMID" cell.Cell.parameters with
  | `String s -> s
  | _ -> raise Expecting_memory_id
  | exception _ -> raise Expecting_memory_id

let is_memrd cell = celltyp cell = "$memrd" 
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
  let mem_cells = List.iter (fun (memid, (rd,wr)) -> Memory.HardCaml.create ~rd ~wr) mems in
  mem_cells

let load_modl techlib (name,modl) = 
  (* find cell in the techlib *)
  let find_cell cell = 
    try List.assoc cell.Y.typ techlib
    with Not_found -> raise Y.(Cell_not_in_techlib(cell.typ, cell.attributes.src))
  in

  (* get cell with explicit inputs and outputs and map to techlib *)
  let mk_cell (inst_name,cell) = 
    let inputs, outputs = 
      List.partition (fun (n,b) ->
        try List.assoc n cell.Y.port_directions = `Input
        with Not_found -> raise (No_cell_direction_specified(cell.Y.typ, n)))
      cell.Y.connections
    in
    inst_name, {
      Cell.typ = cell.Y.typ;
      label = Y.(cell.attributes.src);
      cell = find_cell cell;
      parameters = cell.Y.parameters;
      inputs; outputs;
    }
  in

  (* instantiate all the cells *)
  let open Signal.Comb in

  let get_bus map bus = 
    let bus = List.map net_of_bit bus in
    let find b =
      try I.find b map 
      with Not_found -> raise (Failed_to_find_net(b))
    in
    let simple = false in
    if simple then
      concat @@ List.rev @@ List.map (fun b -> let i,w = find b in w.[i:i]) bus
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
      | [] -> raise Empty_bus
      | h::t -> 
        let i,w = find h in
        let l = opt (w,i,i) t in
        concat @@ List.rev @@ List.map (fun (w,l,h) -> w.[h:l]) l
  in

  let instantiate_cell map (cell_name,cell) co = 
    (* create parameters *)
    let params = List.map (function
      | name,`Int i -> name, Signal.Types.ParamInt i
      | name, `String s -> name, Signal.Types.ParamString s
      | name,_ -> raise (Unsupported_parameter_type(cell_name,name))) cell.Cell.parameters
    in
    (* create input busses *)
    let inputs = List.map (fun (name,bits) -> name, get_bus map bits) cell.Cell.inputs in 
    (* instantiate module *)
    let outputs = cell.Cell.cell params inputs in 
    (* connect outputs *)
    List.iter (fun (n,o) -> S.find n co <== o) outputs
  in

  let instantiate map cells co = List.iter2 (instantiate_cell map) cells co in

  (* connect together wires *)
  let connect_inputs inputs mi = 
    let find n = 
      try S.find n mi
      with Not_found -> raise (Input_not_found(n))
    in
    List.iter (fun (n,i) -> find n <== i) inputs
  in

  let collect_outputs map outputs = 
    List.map (fun (n,(port:Y.port)) -> n, get_bus map port.Y.bits) outputs
  in

  let load_modl modl =
    let cells = List.map mk_cell modl.Y.cells in
    let _ = convert_memories cells in
    let mi,co,map = modl_wire_map modl.Y.ports cells in
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

let load techlib t = 
  List.map (load_modl techlib) t.Y.modl


