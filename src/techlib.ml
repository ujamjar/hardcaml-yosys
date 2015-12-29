open HardCaml

exception Invalid_parameter of string
exception Invalid_input of string

type 'a assoc = (string * 'a) list
type cell = (Signal.Types.parameter assoc -> Signal.Comb.t assoc -> Signal.Comb.t assoc)

let pint = function Signal.Types.ParamInt i -> i 
                  | _ -> raise (Invalid_parameter "expecting int parameter")
let pstr = function Signal.Types.ParamString s -> s 
                  | _ -> raise (Invalid_parameter "expecting string parameter")

module Simlib = struct

  open Signal.Comb

  let (^~:) a b = ~: (a ^: b)

  module Wrapper(P : Interface.S)(I : Interface.S)(O : Interface.S) : sig
    type fn = string * (Signal.Types.parameter P.t -> Signal.Comb.t I.t -> Signal.Comb.t O.t)
    val wrapper : fn -> string * cell
  end = struct
    type fn = string * (Signal.Types.parameter P.t -> Signal.Comb.t I.t -> Signal.Comb.t O.t)
    let of_list_p l = P.map (fun (n,_) -> List.assoc n l) P.t 
    let of_list_i l = I.map (fun (n,_) -> List.assoc n l) I.t 
    let to_list_o o = O.to_list @@ O.map2 (fun (n,_) x -> n,x) O.t o 
    let wrapper (n,f) = 
      n, (fun p i -> to_list_o @@ f (of_list_p p) (of_list_i i))
  end

  module Op1 = struct
    module P = interface A_SIGNED A_WIDTH Y_WIDTH end
    module I = interface A end
    module O = interface Y end
    module W = Wrapper(P)(I)(O)

    let res p = if p.P.a_signed = 1 then sresize else uresize 

    let f1 f p i = 
      let p = P.map pint p in
      assert (width i.I.a = p.P.a_width);
      let a = (res p) i.I.a p.P.y_width in
      O.({ y = f a })

    let not_ = "$not", f1 (~:)
    let pos = "$pos", f1 (fun x -> x)
    let neg = "$neg", f1 negate
    
    let fr f p i = 
      let p = P.map pint p in
      assert (width i.I.a = p.P.a_width);
      O.({ y = uresize (reduce f (bits i.I.a)) p.P.y_width })
    
    let reduce_or = "$reduce_or", fr (|:)
    let reduce_and = "$reduce_and", fr (&:)
    let reduce_xor = "$reduce_xor", fr (^:)
    let reduce_xnor = "$reduce_xnor", 
      (fun p i -> 
        let p = P.map pint p in
        assert (width i.I.a = p.P.a_width);
        let y = reduce (^:) (bits i.I.a) in
        O.({ y = uresize (~: y) p.P.y_width }))
    let reduce_bool = "$reduce_bool", fr (|:)
    
    let logic_not = "$logic_not", 
      (fun p i ->
        let p = P.map pint p in
        assert (width i.I.a = p.P.a_width);
        let y = i.I.a ==:. 0 in
        O.({ y = uresize y p.P.y_width }))

    let cells = [
      not_; pos; neg;
      reduce_or; reduce_and; reduce_xor; 
      reduce_xnor; reduce_bool; logic_not;
    ]

    let get_input_width p = I.({ a = p.P.a_width })
    let get_output_width p = O.({ y = p.P.y_width })
  end
  module Op2 = struct
    module P = interface A_SIGNED B_SIGNED A_WIDTH B_WIDTH Y_WIDTH end
    module I = interface A B end
    module O = interface Y end
    module W = Wrapper(P)(I)(O)

    let res p = if p.P.a_signed = 1 && p.P.b_signed = 1 then sresize else uresize

    let f2 f p i = 
      let p = P.map pint p in
      assert (width i.I.a = p.P.a_width);
      assert (width i.I.b = p.P.b_width);
      let a = (res p) i.I.a p.P.y_width in
      let b = (res p) i.I.b p.P.y_width in
      O.({ y = uresize (f a b) p.P.y_width })

    let and_ = "$and", f2 (&:) 
    let or_ = "$or", f2 (|:)
    let xor_ = "$xor", f2 (^:)
    let xnor_ = "$xnor", f2 (^~:)

    let add = "$add", f2 (+:)
    let sub = "$sub", f2 (-:)
    let mul = "$mul", 
      (fun p i ->
        let p = P.map pint p in
        assert (width i.I.a = p.P.a_width);
        assert (width i.I.b = p.P.b_width);
        let is_signed = p.P.a_signed = 1 && p.P.b_signed = 1 in
        let a = (res p) i.I.a p.P.y_width in
        let b = (res p) i.I.b p.P.y_width in
        let ( *: ) a b = 
          if is_signed then sresize (a *+ b) p.P.y_width 
          else uresize (a *: b) p.P.y_width 
        in
        O.({ y = a *: b }))

    let fs f p i = 
      let p = P.map pint p in
      assert (width i.I.a = p.P.a_width);
      assert (width i.I.b = p.P.b_width);
      let a = (if p.P.a_signed=1 then sresize else uresize) i.I.a (max p.P.y_width p.P.a_width) in
      O.({ y = uresize (f p.P.a_signed a i.I.b) p.P.y_width })

    let shl = "$shl", fs (fun _ a b -> log_shift sll a b)
    let shr = "$shr", fs (fun s a b -> log_shift srl a b)

    let fss f p i = 
      let p = P.map pint p in
      assert (width i.I.a = p.P.a_width);
      assert (width i.I.b = p.P.b_width);
      let a = (if p.P.a_signed=1 then sresize else uresize) i.I.a (max p.P.a_width p.P.y_width) in
      O.({ y = uresize (f p.P.a_signed a i.I.b) p.P.y_width })

    let sshl = "$sshl", fss (fun _ a b -> log_shift sll a b)
    let sshr = "$sshr", fss (fun s a b -> log_shift (if s=1 then sra else srl) a b)

    let shift = "$shift", 
      (fun p i ->
        let p = P.map pint p in
        assert (width i.I.a = p.P.a_width);
        assert (width i.I.b = p.P.b_width);
        let a = uresize i.I.a (max p.P.a_width p.P.y_width) in
        let y = 
          if p.P.b_signed = 1 then
            mux2 (msb i.I.b) 
              (log_shift sll a (negate i.I.b)) 
              (log_shift srl a i.I.b) 
          else
            log_shift srl a i.I.b 
        in
        O.({ y = uresize y p.P.y_width }))

    (* let shiftx = ... *)

    (* let macc = ... *)
    (* let div = ... *)
    (* let mod = ... *)
    (* let pow = ... *)

    let fl f p i = 
      let p = P.map pint p in
      assert (width i.I.a = p.P.a_width);
      assert (width i.I.b = p.P.b_width);
      O.({ y = uresize (f i.I.a i.I.b) p.P.y_width })

    let logic_and = "$logic_and", fl (fun a b -> (a <>:. 0) &: (b <>:. 0))
    let logic_or = "$logic_or", fl (fun a b -> (a <>:. 0) |: (b <>:. 0))

    let fc fs fu p i =
      let p = P.map pint p in
      assert (width i.I.a = p.P.a_width);
      assert (width i.I.b = p.P.b_width);
      let w = max p.P.a_width p.P.b_width in
      let a = (res p) i.I.a w in
      let b = (res p) i.I.b w in
      O.({ y = uresize ((if p.P.a_signed=1 && p.P.b_signed=1 then fs else fu) a b) p.P.y_width })

    let lt = "$lt", fc (<+) (<:)
    let le = "$le", fc (<=+) (<=:)
    let gt = "$gt", fc (>+) (>:)
    let ge = "$ge", fc (>=+) (>=:)
    let eq = "$eq", fc (==:) (==:)
    let ne = "$ne", fc (<>:) (<>:)
    let eqx = "$eqx", fc (==:) (==:)
    let nex = "$nex", fc (<>:) (<>:)

    let cells = [
      and_; or_; xor_; xnor_;
      add; sub; mul;
      shl; shr; sshl; sshr; shift;
      logic_and; logic_or;
      lt; le; gt; ge; eq; ne; eqx; nex;
    ]

    let get_input_width p = I.({ a = p.P.a_width; b = p.P.b_width })
    let get_output_width p = O.({ y = p.P.y_width })
  end

  module Fa = struct
    module P = interface WIDTH end
    module I = interface A B C end
    module O = interface X Y end
    module W = Wrapper(P)(I)(O)

    let fa p i = 
      let wid = pint p.P.width in
      assert (width i.I.a = wid);
      assert (width i.I.b = wid);
      assert (width i.I.c = wid);
      
      let t1 = i.I.a ^: i.I.b in
      let t2 = i.I.a &: i.I.b in
      let t3 = i.I.c &: t1 in
      O.({
        x = t2 |: t3;
        y = t1 ^: i.I.c;
      })
    let fa = "$fa", fa

    let cells = [ fa ]

    let get_input_width p = I.({ a = p.P.width; b = p.P.width; c = p.P.width })
    let get_output_width p = O.({ x = p.P.width; y = p.P.width })
  end

  module Lcu = struct
    module P = interface WIDTH end
    module I = interface P G CI end
    module O = interface CO end
    module W = Wrapper(P)(I)(O)

    let lcu p i = 
      let wid = pint p.P.width in
      assert (width i.I.p = wid);
      assert (width i.I.g = wid);
      assert (width i.I.ci = 1);
      let p = List.rev (bits i.I.p) in
      let g = List.rev (bits i.I.g) in
      let rec f p g ci = 
        match p, g with
        | [], [] -> []
        | p::p', g::g' -> 
          let co = g |: (p &: ci) in
          co :: f p' g' co
        | _ -> failwith "'p' and 'g' list lengths in lcu are not the same"
      in
      let co = concat (List.rev (f p g i.I.ci)) in
      O.({ co })
    let lcu = "$lcu", lcu

    let cells = [ lcu ]

    let get_input_width p = I.({ p = p.P.width; g = p.P.width; ci = 1 })
    let get_output_width p = O.({ co = p.P.width })
  end

  module Slice = struct
    module P = interface OFFSET A_WIDTH Y_WIDTH end
    module I = interface A end
    module O = interface Y end
    module W = Wrapper(P)(I)(O)
    
    let slice p i = 
      let p = P.map pint p in
      assert (width i.I.a = p.P.a_width);
      O.({ y = uresize (srl i.I.a p.P.offset) p.P.y_width })
    let slice = "$slice", slice

    let cells = [ slice ]

    let get_input_width p = I.({ a = p.P.a_width })
    let get_output_width p = O.({ y = p.P.y_width })
  end

  module Concat = struct 
    module P = interface A_WIDTH B_WIDTH end
    module I = interface A B end
    module O = interface Y end
    module W = Wrapper(P)(I)(O)
    
    let concat p i = 
      let p = P.map pint p in
      assert (width i.I.a = p.P.a_width);
      assert (width i.I.b = p.P.b_width);
      O.({ y = i.I.b @: i.I.a })
    let concat = "$concat", concat

    let cells = [ concat ]

    let get_input_width p = I.({ a = p.P.a_width; b = p.P.b_width })
    let get_output_width p = O.({ y = p.P.a_width + p.P.b_width })
  end

  module Mux = struct 
    module P = interface WIDTH end
    module I = interface A B S end
    module O = interface Y end
    module W = Wrapper(P)(I)(O)
    
    let mux p i = 
      let p = P.map pint p in
      assert (width i.I.a = p.P.width);
      assert (width i.I.b = p.P.width);
      assert (width i.I.s = 1);
      O.({ y = mux2 i.I.s i.I.b i.I.a })
    let mux = "$mux", mux

    let cells = [ mux ]

    let get_input_width p = I.({ a = p.P.width; b = p.P.width; s=1 })
    let get_output_width p = O.({ y = p.P.width })
  end

  module Pmux = struct 
    module P = interface WIDTH S_WIDTH end
    module I = interface A B S end
    module O = interface Y end
    module W = Wrapper(P)(I)(O)
    
    let pmux p i = 
      let p = P.map pint p in
      assert (width i.I.a = p.P.width);
      assert (width i.I.b = (p.P.width * p.P.s_width));
      assert (width i.I.s = p.P.s_width);
      let rec pmux s a b i = 
        match s with
        | [] -> a
        | s::t -> 
          let b' = b.[((i+1)*p.P.width)-1:i*p.P.width] in
          mux2 s b' (pmux t a b (i+1))
      in
      O.({ y = pmux (List.rev @@ bits i.I.s) i.I.a i.I.b 0 })
    let pmux = "$pmux", pmux

    let cells = [ pmux ]

    let get_input_width p = I.({ a = p.P.width; b = (p.P.width*p.P.s_width); s=p.P.s_width })
    let get_output_width p = O.({ y = p.P.width })
  end

  module Lut = struct 
    module P = interface WIDTH LUT end
    module I = interface A end
    module O = interface Y end
    module W = Wrapper(P)(I)(O)
    
    let lut p i = 
      let p = P.map pint p in
      assert (width i.I.a = p.P.width);
      let lut = consti (1 lsl p.P.width) p.P.lut in
      let y = mux i.I.a (Array.to_list @@ Array.init (1 lsl p.P.width) (bit lut)) in
      O.({ y })
    let lut = "$lut", lut

    let cells = [ lut ]

    let get_input_width p = I.({ a = p.P.width; })
    let get_output_width p = O.({ y = 1 })
  end 
  (* module Alu = struct .. end *)
  (* module Tribuf = struct .. end *)
  (* module Assert = struct .. end *)
  (* module Assume = struct .. end *)
  (* module Equiv = struct .. end *)
  
  module Seq = Signal.Make_seq(struct 
    let reg_spec = Signal.Seq.r_none 
    let ram_spec = Signal.Seq.r_none 
  end)

  (* module Sr = struct end *)

  module Dff = struct
    module P = interface WIDTH CLK_POLARITY end
    module I = interface CLK D end
    module O = interface Q end
    module W = Wrapper(P)(I)(O)
    let dff p i = 
      let open I in
      let p = P.map pint p in
      assert (width i.d = p.P.width);
      let clkl = if p.P.clk_polarity = 1 then vdd else gnd in
      O.({ q = Seq.reg ~clk:i.clk ~clkl ~e:empty i.d })
    let dff = "$dff", dff
    let cells = [ dff ]
    let get_input_width p = I.({ clk = 1; d = p.P.width })
    let get_output_width p = O.({ q = p.P.width })
  end

  module Dffe = struct
    module P = interface WIDTH CLK_POLARITY EN_POLARITY end
    module I = interface CLK EN D end
    module O = interface Q end
    module W = Wrapper(P)(I)(O)
    let dffe p i = 
      let open I in
      let p = P.map pint p in
      assert (width i.d = p.P.width);
      let clkl = if p.P.clk_polarity = 1 then vdd else gnd in
      let e = if p.P.en_polarity = 1 then i.en else ~: (i.en) in
      O.({ q = Seq.reg ~clk:i.clk ~clkl ~e i.d })
    let dffe = "$dffe", dffe
    let cells = [ dffe ]
    let get_input_width p = I.({ clk = 1; en = 1; d = p.P.width })
    let get_output_width p = O.({ q = p.P.width })
  end

  module Dffsr = struct
    module P = interface WIDTH CLK_POLARITY SET_POLARITY CLR_POLARITY end
    module I = interface CLK SET CLR D end
    module O = interface Q end
    module W = Wrapper(P)(I)(O)
    let dffsr p i = 
      let open I in
      let p = P.map pint p in
      assert (width i.d = p.P.width);
      assert (width i.set = p.P.width);
      assert (width i.clr = p.P.width);
      let clkl = if p.P.clk_polarity = 1 then vdd else gnd in
      let dffsr set clr d = 
        let set = if p.P.set_polarity = 1 then set else ~: set in
        let clr = if p.P.clr_polarity = 1 then clr else ~: clr in
        Seq.reg ~clk:i.clk ~clkl ~r:(set |: clr) ~rv:(mux2 clr gnd vdd) ~e:empty d
      in
      O.({ q = concat @@ List.rev @@ Array.to_list @@
                Array.init p.P.width (fun j -> dffsr i.set.[j:j] i.clr.[j:j] i.d.[j:j]) })
    let dffsr = "$dffsr", dffsr
    let cells = [ dffsr ]
    let get_input_width p = I.({ clk = 1; set = p.P.width; clr = p.P.width; d = p.P.width })
    let get_output_width p = O.({ q = p.P.width })
  end

  module Adff = struct
    module P = interface WIDTH CLK_POLARITY ARST_POLARITY ARST_VALUE end
    module I = interface CLK ARST D end
    module O = interface Q end
    module W = Wrapper(P)(I)(O)
    let adff p i = 
      let open I in
      let p = P.map pint p in
      assert (width i.d = p.P.width);
      let clkl = if p.P.clk_polarity = 1 then vdd else gnd in
      let rl = if p.P.arst_polarity = 1 then vdd else gnd in
      let rv = consti p.P.width p.P.arst_value in
      O.({ q = Seq.reg ~clk:i.clk ~clkl ~r:i.arst ~rl ~rv ~e:empty i.d })
    let adff = "$adff", adff
    let cells = [ adff ]
    let get_input_width p = I.({ clk = 1; arst = 1; d = p.P.width })
    let get_output_width p = O.({ q = p.P.width })
  end

  module Dlatch = struct
    module P = interface WIDTH EN_POLARITY end
    module I = interface EN D end
    module O = interface Q end
    module W = Wrapper(P)(I)(O)
    let get_input_width p = I.({ en = 1; d = p.P.width })
    let get_output_width p = O.({ q = p.P.width })
    let dlatch p i = 
      let open I in
      let p = P.map pint p in
      assert (width i.d = p.P.width);
      let inst = 
      Signal.Instantiation.(inst "dlatch" 
        P.(to_list @@ map2 (fun (n,_) x -> n, Signal.Types.ParamInt x) t p)
        I.(to_list @@ map2 (fun (n,_) x -> n, x) t i)
        O.(to_list @@ map2 (fun (n,_) x -> n, x) t (get_output_width p)))
      in
      O.(map (fun (n,_) -> inst#o n) t)
    let dlatch = "$dlatch", dlatch
    let cells = [ dlatch ]
  end
  
  (* module dlatchsr = struct ... end *)
  (* module fsm = struct ... end *)

  (* module memrd = struct ... end *)
  (* module memwr = struct ... end *)
  (* module meminit = struct ... end *)
  (* module mem = struct ... end *)

  (* must use 'memory -dff' *)
  module Memwr = struct
    module P = interface PRIORITY CLK_POLARITY CLK_ENABLE WIDTH ABITS MEMID end
    module I = interface EN CLK DATA ADDR end
    module O = interface end
    module W = Wrapper(P)(I)(O)
    let get_input_width p = I.({ en = p.P.width; clk = 1; data = p.P.width; addr = p.P.abits })
    let get_output_width p = O.(map snd t)
    let memwr p i = 
      let open I in
      let memid = pstr p.P.memid in
      let p = P.map (fun p -> try pint p with _ -> 0) p in
      assert (width i.en = p.P.width);
      assert (width i.clk = 1);
      assert (width i.data = p.P.width);
      assert (width i.addr = p.P.abits);
      let p' = 
        P.(to_list @@ 
            { map2 (fun (n,_) x -> n, Signal.Types.ParamInt x) t p with
              memid = "MEMID", Signal.Types.ParamString memid })
      in
      let inst = 
      Signal.Instantiation.(inst "memwr" 
        p'
        I.(to_list @@ map2 (fun (n,_) x -> n, x) t i)
        O.(to_list @@ map2 (fun (n,_) x -> n, x) t (get_output_width p)))
      in
      O.(map (fun (n,_) -> inst#o n) t)
    let memwr = "$memwr", memwr
    let cells = [ memwr ]
  end

  module Memrd = struct
    module P = interface TRANSPARENT CLK_POLARITY CLK_ENABLE WIDTH ABITS MEMID end
    module I = interface EN CLK ADDR end
    module O = interface DATA end
    module W = Wrapper(P)(I)(O)
    let get_input_width p = I.({ en = p.P.width; clk = 1; addr = p.P.abits })
    let get_output_width p = O.({ data = p.P.width })
    let memrd p i = 
      let open I in
      let memid = pstr p.P.memid in
      let p = P.map (fun p -> try pint p with _ -> 0) p in
      assert (width i.en = 1);
      assert (width i.clk = 1);
      assert (width i.addr = p.P.abits);
      let p' = 
        P.(to_list @@ 
            { map2 (fun (n,_) x -> n, Signal.Types.ParamInt x) t p with
              memid = "MEMID", Signal.Types.ParamString memid })
      in
      let inst = 
      Signal.Instantiation.(inst "memrd" 
        p'
        I.(to_list @@ map2 (fun (n,_) x -> n, x) t i)
        O.(to_list @@ map2 (fun (n,_) x -> n, x) t (get_output_width p)))
      in
      O.(map (fun (n,_) -> inst#o n) t)
    let memrd = "$memrd", memrd
    let cells = [ memrd ]
  end

  (* 'memory -nobram; opt; clean' *)
  module Mem = struct
    module P = interface 
      ABITS INIT MEMID OFFSET SIZE WIDTH
      RD_CLK_ENABLE RD_CLK_POLARITY RD_PORTS RD_TRANSPARENT 
      WR_CLK_ENABLE WR_CLK_POLARITY WR_PORTS
    end
    module I = interface RD_ADDR RD_CLK RD_EN WR_ADDR WR_CLK WR_DATA WR_EN end
    module O = interface RD_DATA end
    module W = Wrapper(P)(I)(O)

    let get_input_base_width p = 
      I.({ rd_addr = p.P.abits; rd_clk = 1; rd_en = 1;
           wr_addr = p.P.abits; wr_clk = 1; 
           wr_data = p.P.width; wr_en = p.P.width })
    let get_input_ports p = 
      I.({ rd_addr = p.P.rd_ports; rd_clk = p.P.rd_ports; rd_en = p.P.rd_ports;
           wr_addr = p.P.wr_ports; wr_clk = p.P.wr_ports; 
           wr_data = p.P.wr_ports; wr_en = p.P.wr_ports })
    let get_input_width p = I.(map2 ( * ) (get_input_base_width p) (get_input_ports p))

    let get_output_base_width p = O.({ rd_data = p.P.width })
    let get_output_ports p = O.({ rd_data = p.P.rd_ports })
    let get_output_width p = O.(map2 ( * ) (get_output_base_width p) (get_output_ports p))

    let i_to_arrays p i = 
      let bwidth = get_input_base_width p in
      let ports = get_input_ports p in
      let zip = I.(map2 (fun a b -> a,b)) in
      let to_array x (ports,bwidth) =
        assert (width x = (ports * bwidth));
        Array.init ports (fun j -> 
          let l = j * bwidth in
          select x (l+bwidth-1) l)
      in
      I.(map2 to_array i (zip ports bwidth))

    let o_of_arrays o = O.map (fun o -> concat @@ List.rev @@ Array.to_list o) o

    let mem p i = 
      let memid = pstr p.P.memid in
      let p = P.map (fun p -> try pint p with _ -> 0) p in
      let i = i_to_arrays p i in
      O.{ rd_data = zero (p.P.rd_ports * p.P.width) }

    let mem = "$mem", mem
    let cells = [ mem ]
  end

  let cells = 
    (List.map Op1.W.wrapper Op1.cells) @
    (List.map Op2.W.wrapper Op2.cells) @
    (List.map Fa.W.wrapper Fa.cells) @
    (List.map Lcu.W.wrapper Lcu.cells) @
    (List.map Fa.W.wrapper Fa.cells) @
    (List.map Slice.W.wrapper Slice.cells) @
    (List.map Mux.W.wrapper Mux.cells) @
    (List.map Pmux.W.wrapper Pmux.cells) @
    (List.map Lut.W.wrapper Lut.cells) @
    (List.map Dff.W.wrapper Dff.cells) @
    (List.map Dffe.W.wrapper Dffe.cells) @
    (List.map Dffsr.W.wrapper Dffsr.cells) @
    (List.map Adff.W.wrapper Adff.cells) @
    (List.map Dlatch.W.wrapper Dlatch.cells) @
    (List.map Memwr.W.wrapper Memwr.cells) @
    (List.map Memrd.W.wrapper Memrd.cells)

end

module Proof = struct

  module type Cells = sig
    module P : HardCaml.Interface.S
    module I : HardCaml.Interface.S
    module O : HardCaml.Interface.S
    module W : module type of Simlib.Wrapper(P)(I)(O)
    val cells : W.fn list
    val get_input_width : int P.t -> int I.t
    val get_output_width : int P.t -> int O.t
  end

  module Make(C : Cells) = struct

    let cell_name name = String.sub name 1 (String.length name - 1) 
    let tech_name name = "techlib_" ^ cell_name name

    let proof ?(path="") ?(postfix="") params (name,fn) = 

      let open HardCaml.Signal.Comb in

      (*let _ = C.P.(map2 (fun (n,_) x -> Printf.printf "%s: %i\n%!" n x) t params) in*)

      (* circuit inputs *)
      let inputs = 
        let widths = C.get_input_width params in
        C.I.(map2 (fun (n,_) w -> input n w) t widths)
      in

      (* hardcaml simlib circuit *)
      let mkparams p = C.P.(map (fun x -> HardCaml.Signal.Types.ParamInt x) p) in
      let outputs = fn (mkparams params) inputs in

      (*let () = Printf.printf "created %s\n" name in*)

      (* instantiate yosys techlib module *)
      let outputs_chk = 
        let outputs = C.get_output_width params in
        let inst = HardCaml.Signal.Instantiation.inst (tech_name name)
          C.P.(to_list @@ map2 (fun (n,_) p -> n,p) t (mkparams params))
          C.I.(to_list @@ map2 (fun (n,_) i -> n,i) t inputs)
          C.O.(to_list @@ map2 (fun (n,_) o -> n,o) t outputs)
        in
        C.O.(map (fun (n,_) -> inst#o n) t)
      in

      (*let () = Printf.printf "created %s\n" (tech_name name) in*)
      
      (* compare outputs *)
      let check = 
        concat (C.O.to_list outputs) <>: concat (C.O.to_list outputs_chk)
      in

      (* write the test circuit *)
      let name = "sat_" ^ cell_name name ^ postfix in
      let fname = Filename.concat path (name ^ ".v") in
      let yosys = 
        Printf.sprintf 
          "./yosys/yosys-github/yosys -p 'read_verilog -sv test/simlib_chk.v %s; \
          hierarchy -top %s; proc; flatten; sat -prove check 0 -set-def check -set-def-inputs -verify %s' -q -q 2>/dev/null || echo %s failed" 
          fname name name name
      in
      let circ = HardCaml.Circuit.make name [ output "check" check ] in
      let f = open_out fname in
      Printf.fprintf f "// %s\n" yosys;
      HardCaml.Rtl.Verilog.write (output_string f) circ;
      close_out f;
      Printf.printf "%s\n%!" yosys

  end

end

