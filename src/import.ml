open HardCaml
open Printf

exception Cell_not_in_techlib of string * string
exception No_cell_direction_specified of string * string
exception Unsupported_parameter_type of string * string
exception Failed_to_find_net of int
exception Input_not_found of string
exception Empty_bus

module I = Map.Make(struct type t = int let compare = compare end)
module S = Map.Make(struct type t = string let compare = compare end)

type cell = 
  {
    typ : string;
    label : string;
    cell : Techlib.cell;
    parameters : (string * Yosys_atd_t.param_value) list;
    inputs : (string * Yosys_atd_t.bits) list;
    outputs : (string * Yosys_atd_t.bits) list;
  }

type create_fn = HardCaml.Signal.Comb.t Techlib.assoc -> HardCaml.Signal.Comb.t Techlib.assoc

(* something drives a net; it's either a module input, or a cell output. 

    create wires and a map from bits (nets) to wires/indexes for module 
    inputs and cell outputs *)
let modl_wire_map ports cells = 
  let module Y = Yosys_atd_t in

  let map = List.fold_left (fun map (b,s) -> I.add b (0,s) map) I.empty 
    [ 0,Signal.Comb.gnd; 1,Signal.Comb.vdd ]
  in

  (* generate a wire, and create a mapping from the net index to the wire and offet *)
  let map_of_bits map (name,bits) = 
    let wire = Signal.Comb.wire (List.length bits) in
    (name,wire), snd @@ List.fold_left 
      (fun (idx,map) bit ->
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
      (fun map (_,x) -> map_with_smap map_of_bits map x.outputs) 
      map cells 
  in

  mi, co, map

let load techlib t = 
  let module Y = Yosys_atd_t in

  (* find cell in the techlib *)
  let find_cell cell = 
    try List.assoc cell.Y.typ techlib
    with Not_found -> raise Y.(Cell_not_in_techlib(cell.typ, cell.attributes.src))
  in

  (* get cell with explicit inputs and outputs *)
  let mk_cell (inst_name,cell) = 
    let inputs, outputs = 
      List.partition (fun (n,b) ->
        try List.assoc n cell.Y.port_directions = `Input
        with Not_found -> raise (No_cell_direction_specified(cell.Y.typ, n)))
      cell.Y.connections
    in
    inst_name, {
      typ = cell.Y.typ;
      label = Y.(cell.attributes.src);
      cell = find_cell cell;
      parameters = cell.Y.parameters;
      inputs; outputs;
    }
  in

  (* instantiate all the cells *)
  let open Signal.Comb in

  let get_bus map bus = 
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

  let instantiate map cells co = 

    List.iter2 (fun (cell_name,cell) co ->
      
      (* create parameters *)
      let params = List.map (function
        | name,`Int i -> name, Signal.Types.ParamInt i
        | name, `String s -> name, Signal.Types.ParamString s
        | name,_ -> raise (Unsupported_parameter_type(cell_name,name))) cell.parameters
      in
      (* create input busses *)
      let inputs = List.map (fun (name,bits) -> name, get_bus map bits) cell.inputs in 
      (* instantiate module *)
      let outputs = cell.cell params inputs in 
      (* connect outputs *)
      List.iter (fun (n,o) -> S.find n co <== o) outputs) cells co
  in

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
    let mi,co,map = modl_wire_map modl.Y.ports cells in
    instantiate map cells co;
    mi,co,map
  in

  List.map 
    (fun (name,modl) ->

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
      name, (inputs, outputs, create))
    t.Y.modl

