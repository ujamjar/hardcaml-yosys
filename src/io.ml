(* read/write json files *)

let read chan =
  let st = Yojson.init_lexer () in
  let buf = Lexing.from_channel chan in
  Yosys_atd_j.read_t st buf

let write chan t =
  Atdgen_runtime.Util.Json.to_channel Yosys_atd_j.write_t chan t
