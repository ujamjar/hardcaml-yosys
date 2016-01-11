open HardCaml

exception Invalid_parameter of string
exception Invalid_input of string

type 'a assoc = (string * 'a) list
type cell = Signal.Types.parameter assoc -> Signal.Comb.t assoc -> Signal.Comb.t assoc

val pint : Signal.Types.parameter -> int
val pstr : Signal.Types.parameter -> string

module Simlib : sig

  module Wrapper(P : Interface.S)(I : Interface.S)(O : Interface.S) : sig
    type fn = string * (Signal.Types.parameter P.t -> Signal.Comb.t I.t -> Signal.Comb.t O.t)
    val wrapper : fn -> string * cell
  end

  module Op1 : sig
    module P : interface A_SIGNED A_WIDTH Y_WIDTH end
    module I : interface A end
    module O : interface Y end
    module W : module type of Wrapper(P)(I)(O)

    val not_ : W.fn
    val pos : W.fn
    val neg : W.fn
    val reduce_or : W.fn
    val reduce_and : W.fn
    val reduce_xor : W.fn
    val reduce_xnor : W.fn
    val reduce_bool : W.fn
    val logic_not : W.fn

    val cells : W.fn list

    val get_input_width : int P.t -> int I.t
    val get_output_width : int P.t -> int O.t
  end

  module Op2 : sig
    module P : interface A_SIGNED B_SIGNED A_WIDTH B_WIDTH Y_WIDTH end
    module I : interface A B end
    module O : interface Y end
    module W : module type of Wrapper(P)(I)(O)

    val and_ : W.fn
    val or_ : W.fn
    val xor_ : W.fn
    val xnor_ : W.fn

    val add : W.fn
    val sub : W.fn
    val mul : W.fn

    val shl : W.fn
    val shr : W.fn
    val sshl : W.fn
    val sshr : W.fn
    val shift : W.fn

    (* val shiftx, macc, div, mod, pow *)

    val logic_and : W.fn
    val logic_or : W.fn

    val lt : W.fn
    val le : W.fn
    val gt : W.fn
    val ge : W.fn
    val eq : W.fn
    val ne : W.fn
    val eqx : W.fn
    val nex : W.fn

    val cells : W.fn list

    val get_input_width : int P.t -> int I.t
    val get_output_width : int P.t -> int O.t
  end

  module Fa : sig
    module P : interface WIDTH end
    module I : interface A B C end
    module O : interface X Y end
    module W : module type of Wrapper(P)(I)(O)

    val fa : W.fn
    val cells : W.fn list

    val get_input_width : int P.t -> int I.t
    val get_output_width : int P.t -> int O.t
  end

  module Lcu : sig
    module P : interface WIDTH end
    module I : interface P G CI end
    module O : interface CO end
    module W : module type of Wrapper(P)(I)(O)

    val lcu : W.fn
    val cells : W.fn list

    val get_input_width : int P.t -> int I.t
    val get_output_width : int P.t -> int O.t
  end

  (* module Alu : sig ... end *)

  module Slice : sig
    module P : interface OFFSET A_WIDTH Y_WIDTH end
    module I : interface A end
    module O : interface Y end
    module W : module type of Wrapper(P)(I)(O)

    val slice : W.fn
    val cells : W.fn list

    val get_input_width : int P.t -> int I.t
    val get_output_width : int P.t -> int O.t
  end

  module Concat : sig
    module P : interface A_WIDTH B_WIDTH end
    module I : interface A B end
    module O : interface Y end
    module W : module type of Wrapper(P)(I)(O)

    val concat : W.fn
    val cells : W.fn list

    val get_input_width : int P.t -> int I.t
    val get_output_width : int P.t -> int O.t
  end

  module Mux : sig
    module P : interface WIDTH end
    module I : interface A B S end
    module O : interface Y end
    module W : module type of Wrapper(P)(I)(O)

    val mux : W.fn
    val cells : W.fn list

    val get_input_width : int P.t -> int I.t
    val get_output_width : int P.t -> int O.t
  end

  module Pmux : sig
    module P : interface WIDTH S_WIDTH end
    module I : interface A B S end
    module O : interface Y end
    module W : module type of Wrapper(P)(I)(O)

    val pmux : W.fn
    val cells : W.fn list

    val get_input_width : int P.t -> int I.t
    val get_output_width : int P.t -> int O.t
  end

  module Lut : sig
    module P : interface WIDTH LUT end
    module I : interface A end
    module O : interface Y end
    module W : module type of Wrapper(P)(I)(O)

    val lut : W.fn
    val cells : W.fn list

    val get_input_width : int P.t -> int I.t
    val get_output_width : int P.t -> int O.t
  end

  module Dff : sig
    module P : interface WIDTH CLK_POLARITY end
    module I : interface CLK D end
    module O : interface Q end
    module W : module type of Wrapper(P)(I)(O)

    val dff : W.fn
    val cells : W.fn list

    val get_input_width : int P.t -> int I.t
    val get_output_width : int P.t -> int O.t
  end

  module Dffe : sig
    module P : interface WIDTH CLK_POLARITY EN_POLARITY end
    module I : interface CLK EN D end
    module O : interface Q end
    module W : module type of Wrapper(P)(I)(O)

    val dffe : W.fn
    val cells : W.fn list

    val get_input_width : int P.t -> int I.t
    val get_output_width : int P.t -> int O.t
  end

  module Dffsr : sig
    module P : interface WIDTH CLK_POLARITY SET_POLARITY CLR_POLARITY end
    module I : interface CLK SET CLR D end
    module O : interface Q end
    module W : module type of Wrapper(P)(I)(O)

    val dffsr : W.fn
    val cells : W.fn list

    val get_input_width : int P.t -> int I.t
    val get_output_width : int P.t -> int O.t
  end

  module Adff : sig
    module P : interface WIDTH CLK_POLARITY ARST_POLARITY ARST_VALUE end
    module I : interface CLK ARST D end
    module O : interface Q end
    module W : module type of Wrapper(P)(I)(O)

    val adff : W.fn
    val cells : W.fn list

    val get_input_width : int P.t -> int I.t
    val get_output_width : int P.t -> int O.t
  end

  module Memwr : sig
    module P : interface PRIORITY CLK_POLARITY CLK_ENABLE WIDTH ABITS MEMID end
    module I : interface EN CLK DATA ADDR end
    module O : interface end
    module W : module type of Wrapper(P)(I)(O)

    val memwr : W.fn
    val cells : W.fn list

    val get_input_width : int P.t -> int I.t
    val get_output_width : int P.t -> int O.t
  end

  module Memrd : sig
    module P : interface TRANSPARENT CLK_POLARITY CLK_ENABLE WIDTH ABITS MEMID end
    module I : interface EN CLK ADDR end
    module O : interface DATA end
    module W : module type of Wrapper(P)(I)(O)

    val memrd : W.fn
    val cells : W.fn list

    val get_input_width : int P.t -> int I.t
    val get_output_width : int P.t -> int O.t
  end

  module Mem : sig
    module P : interface 
      ABITS INIT MEMID OFFSET SIZE WIDTH
      RD_CLK_ENABLE RD_CLK_POLARITY RD_PORTS RD_TRANSPARENT 
      WR_CLK_ENABLE WR_CLK_POLARITY WR_PORTS
    end
    module I : interface RD_ADDR RD_CLK RD_EN WR_ADDR WR_CLK WR_DATA WR_EN end
    module O : interface RD_DATA end
    module W : module type of Wrapper(P)(I)(O)

    val mem : W.fn
    val cells : W.fn list

    val get_input_width : int P.t -> int I.t
    val get_output_width : int P.t -> int O.t
  end

  (* basic yosys tech library *)
  val cells : string list * cell assoc

end

module Proof : sig

  module type Cells = sig
    module P : HardCaml.Interface.S
    module I : HardCaml.Interface.S
    module O : HardCaml.Interface.S
    module W : module type of Simlib.Wrapper(P)(I)(O)
    val cells : W.fn list
    val get_input_width : int P.t -> int I.t
    val get_output_width : int P.t -> int O.t
  end

  module Make(C : Cells) : sig
    val proof : ?path:string -> ?postfix:string -> int C.P.t -> C.W.fn -> unit
  end

end

