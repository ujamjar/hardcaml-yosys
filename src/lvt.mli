open HardCaml
open Signal
open Comb

module type Cfg = sig
  val abits : int
  val dbits : int
  val size : int
end

module Wr : interface wclk we wa d end
module Rd : interface rclk re ra end

type mode = [ `async_rbw | `async_wbr | `sync_rbw | `sync_wbr ]

val is_async : mode -> bool
val is_sync : mode -> bool
val is_rbw : mode -> bool
val is_wbr : mode -> bool

module Multiport_regs(C : Cfg) : sig

  module Wr : module type of Wr with type 'a t = 'a Wr.t
  module Rd : module type of Rd with type 'a t = 'a Rd.t

  val memory : wr:t Wr.t array -> rd:t Rd.t array -> mode:mode array -> t array

end

module Make(C : Cfg) : sig

  module Wr : module type of Wr with type 'a t = 'a Wr.t
  module Rd : module type of Rd with type 'a t = 'a Rd.t

  val memory : wr:t Wr.t array -> rd: t Rd.t array -> mode:mode array -> t array

end

