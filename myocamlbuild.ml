open Ocamlbuild_plugin

let () = dispatch @@ function
  | Before_options -> Options.use_ocamlfind := true
  | After_rules -> begin
    rule "atdgen t -> ml"
      ~deps:["%.atd"]
      ~prods:["%_t.ml"; "%_t.mli"]
      (fun env _ ->
        let inp = env "%.atd" in
        let outp = env "%" in
        Cmd(S[A"atdgen"; A"-t"; A"-o"; A outp; A inp])
      );
    rule "atdgen j -> ml"
      ~deps:["%.atd"]
      ~prods:["%_j.ml"; "%_j.mli"]
      (fun env _ ->
        let inp = env "%.atd" in
        let outp = env "%" in
        Cmd(S[A"atdgen"; A"-j"; A"-j-std"; A"-j-strict-fields"; A"-o"; A outp; A inp])
      )
  end
  | _ -> ()


