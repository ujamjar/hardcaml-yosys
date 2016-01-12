open HardCaml
open Signal
open Comb

module type Cfg = sig
  val abits : int
  val dbits : int
  val size : int
end

module Wr : interface we wa d end
module Rd : interface re ra end

type mode = [ `async_rbw | `async_wbr | `sync_rbw | `sync_wbr ]

val is_async : mode -> bool
val is_sync : mode -> bool
val is_rbw : mode -> bool
val is_wbr : mode -> bool

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

module Multiport_regs(C : Cfg) : sig

  module Wr : module type of Wr with type 'a t = 'a Wr.t
  module Rd : module type of Rd with type 'a t = 'a Rd.t

  val memory : wr:wr_port array -> rd:rd_port array -> t array

end

module Make(C : Cfg) : sig

  module Wr : module type of Wr with type 'a t = 'a Wr.t
  module Rd : module type of Rd with type 'a t = 'a Rd.t

  val memory : wr:wr_port array -> rd:rd_port array -> t array

end

module Make_wren(C : Cfg) : sig

  module Wr : module type of Wr with type 'a t = 'a Wr.t
  module Rd : module type of Rd with type 'a t = 'a Rd.t

  val memory : layout:int array array -> 
    wr:wr_port array -> rd:rd_port array -> t array

end

