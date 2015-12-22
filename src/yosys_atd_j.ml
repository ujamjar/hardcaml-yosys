(* Auto-generated from "yosys_atd.atd" *)


type dyn = Yojson.Safe.json

type direction = Yosys_atd_t.direction

type bits = Yosys_atd_t.bits

type port = Yosys_atd_t.port = { direction: direction; bits: bits }

type param_value = Yosys_atd_t.param_value

type attributes = Yosys_atd_t.attributes = { src: string }

type netname = Yosys_atd_t.netname = {
  hide_name: int;
  bits: bits;
  attributes: attributes
}

type cell = Yosys_atd_t.cell = {
  hide_name: int;
  typ: string;
  parameters: (string * param_value) list;
  attributes: attributes;
  port_directions: (string * direction) list;
  connections: (string * bits) list
}

type modl = Yosys_atd_t.modl = {
  ports: (string * port) list;
  cells: (string * cell) list;
  netnames: (string * netname) list
}

type t = Yosys_atd_t.t = { creator: string; modl: (string * modl) list }

let write_dyn = (
  Yojson.Safe.write_json
)
let string_of_dyn ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_dyn ob x;
  Bi_outbuf.contents ob
let read_dyn = (
  Yojson.Safe.read_json
)
let dyn_of_string s =
  read_dyn (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_direction = (
  fun ob sum ->
    match sum with
      | `Input -> Bi_outbuf.add_string ob "\"input\""
      | `Output -> Bi_outbuf.add_string ob "\"output\""
)
let string_of_direction ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_direction ob x;
  Bi_outbuf.contents ob
let read_direction = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          Yojson.Safe.read_space p lb;
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              try
                match len with
                  | 5 -> (
                      if String.unsafe_get s pos = 'i' && String.unsafe_get s (pos+1) = 'n' && String.unsafe_get s (pos+2) = 'p' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 't' then (
                        0
                      )
                      else (
                        raise (Exit)
                      )
                    )
                  | 6 -> (
                      if String.unsafe_get s pos = 'o' && String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'p' && String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 't' then (
                        1
                      )
                      else (
                        raise (Exit)
                      )
                    )
                  | _ -> (
                      raise (Exit)
                    )
              with Exit -> (
                  Ag_oj_run.invalid_variant_tag p (String.sub s pos len)
                )
          in
          let i = Yojson.Safe.map_ident p f lb in
          match i with
            | 0 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `Input
            | 1 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `Output
            | _ -> (
                assert false
              )
        )
      | `Double_quote -> (
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              try
                match len with
                  | 5 -> (
                      if String.unsafe_get s pos = 'i' && String.unsafe_get s (pos+1) = 'n' && String.unsafe_get s (pos+2) = 'p' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 't' then (
                        0
                      )
                      else (
                        raise (Exit)
                      )
                    )
                  | 6 -> (
                      if String.unsafe_get s pos = 'o' && String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'p' && String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 't' then (
                        1
                      )
                      else (
                        raise (Exit)
                      )
                    )
                  | _ -> (
                      raise (Exit)
                    )
              with Exit -> (
                  Ag_oj_run.invalid_variant_tag p (String.sub s pos len)
                )
          in
          let i = Yojson.Safe.map_string p f lb in
          match i with
            | 0 ->
              `Input
            | 1 ->
              `Output
            | _ -> (
                assert false
              )
        )
      | `Square_bracket -> (
          Yojson.Safe.read_space p lb;
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              Ag_oj_run.invalid_variant_tag p (String.sub s pos len)
          in
          let i = Yojson.Safe.map_ident p f lb in
          match i with
            | _ -> (
                assert false
              )
        )
)
let direction_of_string s =
  read_direction (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__1 = (
  Ag_oj_run.write_list (
    write_dyn
  )
)
let string_of__1 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__1 ob x;
  Bi_outbuf.contents ob
let read__1 = (
  Ag_oj_run.read_list (
    read_dyn
  )
)
let _1_of_string s =
  read__1 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_bits = (
  write__1
)
let string_of_bits ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_bits ob x;
  Bi_outbuf.contents ob
let read_bits = (
  read__1
)
let bits_of_string s =
  read_bits (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_port : _ -> port -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"direction\":";
    (
      write_direction
    )
      ob x.direction;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"bits\":";
    (
      write_bits
    )
      ob x.bits;
    Bi_outbuf.add_char ob '}';
)
let string_of_port ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_port ob x;
  Bi_outbuf.contents ob
let read_port = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_direction = ref (Obj.magic 0.0) in
    let field_bits = ref (Obj.magic 0.0) in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          match len with
            | 4 -> (
                if String.unsafe_get s pos = 'b' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 's' then (
                  1
                )
                else (
                  (!Ag_util.Json.unknown_field_handler) "File \"yosys_atd.atd\", line 7, characters 2-51" (String.sub s pos len); -1
                )
              )
            | 9 -> (
                if String.unsafe_get s pos = 'd' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 'c' && String.unsafe_get s (pos+5) = 't' && String.unsafe_get s (pos+6) = 'i' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'n' then (
                  0
                )
                else (
                  (!Ag_util.Json.unknown_field_handler) "File \"yosys_atd.atd\", line 7, characters 2-51" (String.sub s pos len); -1
                )
              )
            | _ -> (
                (!Ag_util.Json.unknown_field_handler) "File \"yosys_atd.atd\", line 7, characters 2-51" (String.sub s pos len); -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Ag_oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_direction := (
              (
                read_direction
              ) p lb
            );
            bits0 := !bits0 lor 0x1;
          | 1 ->
            field_bits := (
              (
                read_bits
              ) p lb
            );
            bits0 := !bits0 lor 0x2;
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            match len with
              | 4 -> (
                  if String.unsafe_get s pos = 'b' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 's' then (
                    1
                  )
                  else (
                    (!Ag_util.Json.unknown_field_handler) "File \"yosys_atd.atd\", line 7, characters 2-51" (String.sub s pos len); -1
                  )
                )
              | 9 -> (
                  if String.unsafe_get s pos = 'd' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 'c' && String.unsafe_get s (pos+5) = 't' && String.unsafe_get s (pos+6) = 'i' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'n' then (
                    0
                  )
                  else (
                    (!Ag_util.Json.unknown_field_handler) "File \"yosys_atd.atd\", line 7, characters 2-51" (String.sub s pos len); -1
                  )
                )
              | _ -> (
                  (!Ag_util.Json.unknown_field_handler) "File \"yosys_atd.atd\", line 7, characters 2-51" (String.sub s pos len); -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Ag_oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_direction := (
                (
                  read_direction
                ) p lb
              );
              bits0 := !bits0 lor 0x1;
            | 1 ->
              field_bits := (
                (
                  read_bits
                ) p lb
              );
              bits0 := !bits0 lor 0x2;
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x3 then Ag_oj_run.missing_fields p [| !bits0 |] [| "direction"; "bits" |];
        (
          {
            direction = !field_direction;
            bits = !field_bits;
          }
         : port)
      )
)
let port_of_string s =
  read_port (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_param_value = (
  write_dyn
)
let string_of_param_value ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_param_value ob x;
  Bi_outbuf.contents ob
let read_param_value = (
  read_dyn
)
let param_value_of_string s =
  read_param_value (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_attributes : _ -> attributes -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if x.src <> "" then (
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"src\":";
      (
        Yojson.Safe.write_string
      )
        ob x.src;
    );
    Bi_outbuf.add_char ob '}';
)
let string_of_attributes ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_attributes ob x;
  Bi_outbuf.contents ob
let read_attributes = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_src = ref ("") in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          if len = 3 && String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'c' then (
            0
          )
          else (
            (!Ag_util.Json.unknown_field_handler) "File \"yosys_atd.atd\", line 13, characters 2-26" (String.sub s pos len); -1
          )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Ag_oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_src := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
            )
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            if len = 3 && String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'c' then (
              0
            )
            else (
              (!Ag_util.Json.unknown_field_handler) "File \"yosys_atd.atd\", line 13, characters 2-26" (String.sub s pos len); -1
            )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Ag_oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_src := (
                  (
                    Ag_oj_run.read_string
                  ) p lb
                );
              )
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            src = !field_src;
          }
         : attributes)
      )
)
let attributes_of_string s =
  read_attributes (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_netname : _ -> netname -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"hide_name\":";
    (
      Yojson.Safe.write_int
    )
      ob x.hide_name;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"bits\":";
    (
      write_bits
    )
      ob x.bits;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"attributes\":";
    (
      write_attributes
    )
      ob x.attributes;
    Bi_outbuf.add_char ob '}';
)
let string_of_netname ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_netname ob x;
  Bi_outbuf.contents ob
let read_netname = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_hide_name = ref (Obj.magic 0.0) in
    let field_bits = ref (Obj.magic 0.0) in
    let field_attributes = ref (Obj.magic 0.0) in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          match len with
            | 4 -> (
                if String.unsafe_get s pos = 'b' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 's' then (
                  1
                )
                else (
                  (!Ag_util.Json.unknown_field_handler) "File \"yosys_atd.atd\", line 28, characters 2-74" (String.sub s pos len); -1
                )
              )
            | 9 -> (
                if String.unsafe_get s pos = 'h' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'd' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'n' && String.unsafe_get s (pos+6) = 'a' && String.unsafe_get s (pos+7) = 'm' && String.unsafe_get s (pos+8) = 'e' then (
                  0
                )
                else (
                  (!Ag_util.Json.unknown_field_handler) "File \"yosys_atd.atd\", line 28, characters 2-74" (String.sub s pos len); -1
                )
              )
            | 10 -> (
                if String.unsafe_get s pos = 'a' && String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'b' && String.unsafe_get s (pos+6) = 'u' && String.unsafe_get s (pos+7) = 't' && String.unsafe_get s (pos+8) = 'e' && String.unsafe_get s (pos+9) = 's' then (
                  2
                )
                else (
                  (!Ag_util.Json.unknown_field_handler) "File \"yosys_atd.atd\", line 28, characters 2-74" (String.sub s pos len); -1
                )
              )
            | _ -> (
                (!Ag_util.Json.unknown_field_handler) "File \"yosys_atd.atd\", line 28, characters 2-74" (String.sub s pos len); -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Ag_oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_hide_name := (
              (
                Ag_oj_run.read_int
              ) p lb
            );
            bits0 := !bits0 lor 0x1;
          | 1 ->
            field_bits := (
              (
                read_bits
              ) p lb
            );
            bits0 := !bits0 lor 0x2;
          | 2 ->
            field_attributes := (
              (
                read_attributes
              ) p lb
            );
            bits0 := !bits0 lor 0x4;
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            match len with
              | 4 -> (
                  if String.unsafe_get s pos = 'b' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 's' then (
                    1
                  )
                  else (
                    (!Ag_util.Json.unknown_field_handler) "File \"yosys_atd.atd\", line 28, characters 2-74" (String.sub s pos len); -1
                  )
                )
              | 9 -> (
                  if String.unsafe_get s pos = 'h' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'd' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'n' && String.unsafe_get s (pos+6) = 'a' && String.unsafe_get s (pos+7) = 'm' && String.unsafe_get s (pos+8) = 'e' then (
                    0
                  )
                  else (
                    (!Ag_util.Json.unknown_field_handler) "File \"yosys_atd.atd\", line 28, characters 2-74" (String.sub s pos len); -1
                  )
                )
              | 10 -> (
                  if String.unsafe_get s pos = 'a' && String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'b' && String.unsafe_get s (pos+6) = 'u' && String.unsafe_get s (pos+7) = 't' && String.unsafe_get s (pos+8) = 'e' && String.unsafe_get s (pos+9) = 's' then (
                    2
                  )
                  else (
                    (!Ag_util.Json.unknown_field_handler) "File \"yosys_atd.atd\", line 28, characters 2-74" (String.sub s pos len); -1
                  )
                )
              | _ -> (
                  (!Ag_util.Json.unknown_field_handler) "File \"yosys_atd.atd\", line 28, characters 2-74" (String.sub s pos len); -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Ag_oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_hide_name := (
                (
                  Ag_oj_run.read_int
                ) p lb
              );
              bits0 := !bits0 lor 0x1;
            | 1 ->
              field_bits := (
                (
                  read_bits
                ) p lb
              );
              bits0 := !bits0 lor 0x2;
            | 2 ->
              field_attributes := (
                (
                  read_attributes
                ) p lb
              );
              bits0 := !bits0 lor 0x4;
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x7 then Ag_oj_run.missing_fields p [| !bits0 |] [| "hide_name"; "bits"; "attributes" |];
        (
          {
            hide_name = !field_hide_name;
            bits = !field_bits;
            attributes = !field_attributes;
          }
         : netname)
      )
)
let netname_of_string s =
  read_netname (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__4 = (
  Ag_oj_run.write_assoc_list (
    Yojson.Safe.write_string
  ) (
    write_bits
  )
)
let string_of__4 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__4 ob x;
  Bi_outbuf.contents ob
let read__4 = (
  Ag_oj_run.read_assoc_list (
    Ag_oj_run.read_string
  ) (
    read_bits
  )
)
let _4_of_string s =
  read__4 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__3 = (
  Ag_oj_run.write_assoc_list (
    Yojson.Safe.write_string
  ) (
    write_direction
  )
)
let string_of__3 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__3 ob x;
  Bi_outbuf.contents ob
let read__3 = (
  Ag_oj_run.read_assoc_list (
    Ag_oj_run.read_string
  ) (
    read_direction
  )
)
let _3_of_string s =
  read__3 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__2 = (
  Ag_oj_run.write_assoc_list (
    Yojson.Safe.write_string
  ) (
    write_param_value
  )
)
let string_of__2 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__2 ob x;
  Bi_outbuf.contents ob
let read__2 = (
  Ag_oj_run.read_assoc_list (
    Ag_oj_run.read_string
  ) (
    read_param_value
  )
)
let _2_of_string s =
  read__2 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_cell : _ -> cell -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"hide_name\":";
    (
      Yojson.Safe.write_int
    )
      ob x.hide_name;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"type\":";
    (
      Yojson.Safe.write_string
    )
      ob x.typ;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"parameters\":";
    (
      write__2
    )
      ob x.parameters;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"attributes\":";
    (
      write_attributes
    )
      ob x.attributes;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"port_directions\":";
    (
      write__3
    )
      ob x.port_directions;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"connections\":";
    (
      write__4
    )
      ob x.connections;
    Bi_outbuf.add_char ob '}';
)
let string_of_cell ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_cell ob x;
  Bi_outbuf.contents ob
let read_cell = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_hide_name = ref (Obj.magic 0.0) in
    let field_typ = ref (Obj.magic 0.0) in
    let field_parameters = ref (Obj.magic 0.0) in
    let field_attributes = ref (Obj.magic 0.0) in
    let field_port_directions = ref (Obj.magic 0.0) in
    let field_connections = ref (Obj.magic 0.0) in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          match len with
            | 4 -> (
                if String.unsafe_get s pos = 't' && String.unsafe_get s (pos+1) = 'y' && String.unsafe_get s (pos+2) = 'p' && String.unsafe_get s (pos+3) = 'e' then (
                  1
                )
                else (
                  (!Ag_util.Json.unknown_field_handler) "File \"yosys_atd.atd\", line 18, characters 2-292" (String.sub s pos len); -1
                )
              )
            | 9 -> (
                if String.unsafe_get s pos = 'h' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'd' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'n' && String.unsafe_get s (pos+6) = 'a' && String.unsafe_get s (pos+7) = 'm' && String.unsafe_get s (pos+8) = 'e' then (
                  0
                )
                else (
                  (!Ag_util.Json.unknown_field_handler) "File \"yosys_atd.atd\", line 18, characters 2-292" (String.sub s pos len); -1
                )
              )
            | 10 -> (
                match String.unsafe_get s pos with
                  | 'a' -> (
                      if String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'b' && String.unsafe_get s (pos+6) = 'u' && String.unsafe_get s (pos+7) = 't' && String.unsafe_get s (pos+8) = 'e' && String.unsafe_get s (pos+9) = 's' then (
                        3
                      )
                      else (
                        (!Ag_util.Json.unknown_field_handler) "File \"yosys_atd.atd\", line 18, characters 2-292" (String.sub s pos len); -1
                      )
                    )
                  | 'p' -> (
                      if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'm' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = 't' && String.unsafe_get s (pos+7) = 'e' && String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 's' then (
                        2
                      )
                      else (
                        (!Ag_util.Json.unknown_field_handler) "File \"yosys_atd.atd\", line 18, characters 2-292" (String.sub s pos len); -1
                      )
                    )
                  | _ -> (
                      (!Ag_util.Json.unknown_field_handler) "File \"yosys_atd.atd\", line 18, characters 2-292" (String.sub s pos len); -1
                    )
              )
            | 11 -> (
                if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'n' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = 'c' && String.unsafe_get s (pos+6) = 't' && String.unsafe_get s (pos+7) = 'i' && String.unsafe_get s (pos+8) = 'o' && String.unsafe_get s (pos+9) = 'n' && String.unsafe_get s (pos+10) = 's' then (
                  5
                )
                else (
                  (!Ag_util.Json.unknown_field_handler) "File \"yosys_atd.atd\", line 18, characters 2-292" (String.sub s pos len); -1
                )
              )
            | 15 -> (
                if String.unsafe_get s pos = 'p' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'd' && String.unsafe_get s (pos+6) = 'i' && String.unsafe_get s (pos+7) = 'r' && String.unsafe_get s (pos+8) = 'e' && String.unsafe_get s (pos+9) = 'c' && String.unsafe_get s (pos+10) = 't' && String.unsafe_get s (pos+11) = 'i' && String.unsafe_get s (pos+12) = 'o' && String.unsafe_get s (pos+13) = 'n' && String.unsafe_get s (pos+14) = 's' then (
                  4
                )
                else (
                  (!Ag_util.Json.unknown_field_handler) "File \"yosys_atd.atd\", line 18, characters 2-292" (String.sub s pos len); -1
                )
              )
            | _ -> (
                (!Ag_util.Json.unknown_field_handler) "File \"yosys_atd.atd\", line 18, characters 2-292" (String.sub s pos len); -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Ag_oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_hide_name := (
              (
                Ag_oj_run.read_int
              ) p lb
            );
            bits0 := !bits0 lor 0x1;
          | 1 ->
            field_typ := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x2;
          | 2 ->
            field_parameters := (
              (
                read__2
              ) p lb
            );
            bits0 := !bits0 lor 0x4;
          | 3 ->
            field_attributes := (
              (
                read_attributes
              ) p lb
            );
            bits0 := !bits0 lor 0x8;
          | 4 ->
            field_port_directions := (
              (
                read__3
              ) p lb
            );
            bits0 := !bits0 lor 0x10;
          | 5 ->
            field_connections := (
              (
                read__4
              ) p lb
            );
            bits0 := !bits0 lor 0x20;
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            match len with
              | 4 -> (
                  if String.unsafe_get s pos = 't' && String.unsafe_get s (pos+1) = 'y' && String.unsafe_get s (pos+2) = 'p' && String.unsafe_get s (pos+3) = 'e' then (
                    1
                  )
                  else (
                    (!Ag_util.Json.unknown_field_handler) "File \"yosys_atd.atd\", line 18, characters 2-292" (String.sub s pos len); -1
                  )
                )
              | 9 -> (
                  if String.unsafe_get s pos = 'h' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'd' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'n' && String.unsafe_get s (pos+6) = 'a' && String.unsafe_get s (pos+7) = 'm' && String.unsafe_get s (pos+8) = 'e' then (
                    0
                  )
                  else (
                    (!Ag_util.Json.unknown_field_handler) "File \"yosys_atd.atd\", line 18, characters 2-292" (String.sub s pos len); -1
                  )
                )
              | 10 -> (
                  match String.unsafe_get s pos with
                    | 'a' -> (
                        if String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'b' && String.unsafe_get s (pos+6) = 'u' && String.unsafe_get s (pos+7) = 't' && String.unsafe_get s (pos+8) = 'e' && String.unsafe_get s (pos+9) = 's' then (
                          3
                        )
                        else (
                          (!Ag_util.Json.unknown_field_handler) "File \"yosys_atd.atd\", line 18, characters 2-292" (String.sub s pos len); -1
                        )
                      )
                    | 'p' -> (
                        if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'm' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = 't' && String.unsafe_get s (pos+7) = 'e' && String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 's' then (
                          2
                        )
                        else (
                          (!Ag_util.Json.unknown_field_handler) "File \"yosys_atd.atd\", line 18, characters 2-292" (String.sub s pos len); -1
                        )
                      )
                    | _ -> (
                        (!Ag_util.Json.unknown_field_handler) "File \"yosys_atd.atd\", line 18, characters 2-292" (String.sub s pos len); -1
                      )
                )
              | 11 -> (
                  if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'n' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = 'c' && String.unsafe_get s (pos+6) = 't' && String.unsafe_get s (pos+7) = 'i' && String.unsafe_get s (pos+8) = 'o' && String.unsafe_get s (pos+9) = 'n' && String.unsafe_get s (pos+10) = 's' then (
                    5
                  )
                  else (
                    (!Ag_util.Json.unknown_field_handler) "File \"yosys_atd.atd\", line 18, characters 2-292" (String.sub s pos len); -1
                  )
                )
              | 15 -> (
                  if String.unsafe_get s pos = 'p' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'd' && String.unsafe_get s (pos+6) = 'i' && String.unsafe_get s (pos+7) = 'r' && String.unsafe_get s (pos+8) = 'e' && String.unsafe_get s (pos+9) = 'c' && String.unsafe_get s (pos+10) = 't' && String.unsafe_get s (pos+11) = 'i' && String.unsafe_get s (pos+12) = 'o' && String.unsafe_get s (pos+13) = 'n' && String.unsafe_get s (pos+14) = 's' then (
                    4
                  )
                  else (
                    (!Ag_util.Json.unknown_field_handler) "File \"yosys_atd.atd\", line 18, characters 2-292" (String.sub s pos len); -1
                  )
                )
              | _ -> (
                  (!Ag_util.Json.unknown_field_handler) "File \"yosys_atd.atd\", line 18, characters 2-292" (String.sub s pos len); -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Ag_oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_hide_name := (
                (
                  Ag_oj_run.read_int
                ) p lb
              );
              bits0 := !bits0 lor 0x1;
            | 1 ->
              field_typ := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x2;
            | 2 ->
              field_parameters := (
                (
                  read__2
                ) p lb
              );
              bits0 := !bits0 lor 0x4;
            | 3 ->
              field_attributes := (
                (
                  read_attributes
                ) p lb
              );
              bits0 := !bits0 lor 0x8;
            | 4 ->
              field_port_directions := (
                (
                  read__3
                ) p lb
              );
              bits0 := !bits0 lor 0x10;
            | 5 ->
              field_connections := (
                (
                  read__4
                ) p lb
              );
              bits0 := !bits0 lor 0x20;
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x3f then Ag_oj_run.missing_fields p [| !bits0 |] [| "hide_name"; "typ"; "parameters"; "attributes"; "port_directions"; "connections" |];
        (
          {
            hide_name = !field_hide_name;
            typ = !field_typ;
            parameters = !field_parameters;
            attributes = !field_attributes;
            port_directions = !field_port_directions;
            connections = !field_connections;
          }
         : cell)
      )
)
let cell_of_string s =
  read_cell (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__7 = (
  Ag_oj_run.write_assoc_list (
    Yojson.Safe.write_string
  ) (
    write_netname
  )
)
let string_of__7 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__7 ob x;
  Bi_outbuf.contents ob
let read__7 = (
  Ag_oj_run.read_assoc_list (
    Ag_oj_run.read_string
  ) (
    read_netname
  )
)
let _7_of_string s =
  read__7 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__6 = (
  Ag_oj_run.write_assoc_list (
    Yojson.Safe.write_string
  ) (
    write_cell
  )
)
let string_of__6 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__6 ob x;
  Bi_outbuf.contents ob
let read__6 = (
  Ag_oj_run.read_assoc_list (
    Ag_oj_run.read_string
  ) (
    read_cell
  )
)
let _6_of_string s =
  read__6 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__5 = (
  Ag_oj_run.write_assoc_list (
    Yojson.Safe.write_string
  ) (
    write_port
  )
)
let string_of__5 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__5 ob x;
  Bi_outbuf.contents ob
let read__5 = (
  Ag_oj_run.read_assoc_list (
    Ag_oj_run.read_string
  ) (
    read_port
  )
)
let _5_of_string s =
  read__5 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_modl : _ -> modl -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"ports\":";
    (
      write__5
    )
      ob x.ports;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"cells\":";
    (
      write__6
    )
      ob x.cells;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"netnames\":";
    (
      write__7
    )
      ob x.netnames;
    Bi_outbuf.add_char ob '}';
)
let string_of_modl ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_modl ob x;
  Bi_outbuf.contents ob
let read_modl = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_ports = ref (Obj.magic 0.0) in
    let field_cells = ref (Obj.magic 0.0) in
    let field_netnames = ref (Obj.magic 0.0) in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          match len with
            | 5 -> (
                match String.unsafe_get s pos with
                  | 'c' -> (
                      if String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 's' then (
                        1
                      )
                      else (
                        (!Ag_util.Json.unknown_field_handler) "File \"yosys_atd.atd\", line 35, characters 2-178" (String.sub s pos len); -1
                      )
                    )
                  | 'p' -> (
                      if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 's' then (
                        0
                      )
                      else (
                        (!Ag_util.Json.unknown_field_handler) "File \"yosys_atd.atd\", line 35, characters 2-178" (String.sub s pos len); -1
                      )
                    )
                  | _ -> (
                      (!Ag_util.Json.unknown_field_handler) "File \"yosys_atd.atd\", line 35, characters 2-178" (String.sub s pos len); -1
                    )
              )
            | 8 -> (
                if String.unsafe_get s pos = 'n' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'n' && String.unsafe_get s (pos+4) = 'a' && String.unsafe_get s (pos+5) = 'm' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 's' then (
                  2
                )
                else (
                  (!Ag_util.Json.unknown_field_handler) "File \"yosys_atd.atd\", line 35, characters 2-178" (String.sub s pos len); -1
                )
              )
            | _ -> (
                (!Ag_util.Json.unknown_field_handler) "File \"yosys_atd.atd\", line 35, characters 2-178" (String.sub s pos len); -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Ag_oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_ports := (
              (
                read__5
              ) p lb
            );
            bits0 := !bits0 lor 0x1;
          | 1 ->
            field_cells := (
              (
                read__6
              ) p lb
            );
            bits0 := !bits0 lor 0x2;
          | 2 ->
            field_netnames := (
              (
                read__7
              ) p lb
            );
            bits0 := !bits0 lor 0x4;
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            match len with
              | 5 -> (
                  match String.unsafe_get s pos with
                    | 'c' -> (
                        if String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 's' then (
                          1
                        )
                        else (
                          (!Ag_util.Json.unknown_field_handler) "File \"yosys_atd.atd\", line 35, characters 2-178" (String.sub s pos len); -1
                        )
                      )
                    | 'p' -> (
                        if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 's' then (
                          0
                        )
                        else (
                          (!Ag_util.Json.unknown_field_handler) "File \"yosys_atd.atd\", line 35, characters 2-178" (String.sub s pos len); -1
                        )
                      )
                    | _ -> (
                        (!Ag_util.Json.unknown_field_handler) "File \"yosys_atd.atd\", line 35, characters 2-178" (String.sub s pos len); -1
                      )
                )
              | 8 -> (
                  if String.unsafe_get s pos = 'n' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'n' && String.unsafe_get s (pos+4) = 'a' && String.unsafe_get s (pos+5) = 'm' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 's' then (
                    2
                  )
                  else (
                    (!Ag_util.Json.unknown_field_handler) "File \"yosys_atd.atd\", line 35, characters 2-178" (String.sub s pos len); -1
                  )
                )
              | _ -> (
                  (!Ag_util.Json.unknown_field_handler) "File \"yosys_atd.atd\", line 35, characters 2-178" (String.sub s pos len); -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Ag_oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_ports := (
                (
                  read__5
                ) p lb
              );
              bits0 := !bits0 lor 0x1;
            | 1 ->
              field_cells := (
                (
                  read__6
                ) p lb
              );
              bits0 := !bits0 lor 0x2;
            | 2 ->
              field_netnames := (
                (
                  read__7
                ) p lb
              );
              bits0 := !bits0 lor 0x4;
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x7 then Ag_oj_run.missing_fields p [| !bits0 |] [| "ports"; "cells"; "netnames" |];
        (
          {
            ports = !field_ports;
            cells = !field_cells;
            netnames = !field_netnames;
          }
         : modl)
      )
)
let modl_of_string s =
  read_modl (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__8 = (
  Ag_oj_run.write_assoc_list (
    Yojson.Safe.write_string
  ) (
    write_modl
  )
)
let string_of__8 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__8 ob x;
  Bi_outbuf.contents ob
let read__8 = (
  Ag_oj_run.read_assoc_list (
    Ag_oj_run.read_string
  ) (
    read_modl
  )
)
let _8_of_string s =
  read__8 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_t : _ -> t -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"creator\":";
    (
      Yojson.Safe.write_string
    )
      ob x.creator;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"modules\":";
    (
      write__8
    )
      ob x.modl;
    Bi_outbuf.add_char ob '}';
)
let string_of_t ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_t ob x;
  Bi_outbuf.contents ob
let read_t = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_creator = ref (Obj.magic 0.0) in
    let field_modl = ref (Obj.magic 0.0) in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          if len = 7 then (
            match String.unsafe_get s pos with
              | 'c' -> (
                  if String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 't' && String.unsafe_get s (pos+5) = 'o' && String.unsafe_get s (pos+6) = 'r' then (
                    0
                  )
                  else (
                    (!Ag_util.Json.unknown_field_handler) "File \"yosys_atd.atd\", line 42, characters 2-105" (String.sub s pos len); -1
                  )
                )
              | 'm' -> (
                  if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'd' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'l' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = 's' then (
                    1
                  )
                  else (
                    (!Ag_util.Json.unknown_field_handler) "File \"yosys_atd.atd\", line 42, characters 2-105" (String.sub s pos len); -1
                  )
                )
              | _ -> (
                  (!Ag_util.Json.unknown_field_handler) "File \"yosys_atd.atd\", line 42, characters 2-105" (String.sub s pos len); -1
                )
          )
          else (
            (!Ag_util.Json.unknown_field_handler) "File \"yosys_atd.atd\", line 42, characters 2-105" (String.sub s pos len); -1
          )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Ag_oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_creator := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x1;
          | 1 ->
            field_modl := (
              (
                read__8
              ) p lb
            );
            bits0 := !bits0 lor 0x2;
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            if len = 7 then (
              match String.unsafe_get s pos with
                | 'c' -> (
                    if String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 't' && String.unsafe_get s (pos+5) = 'o' && String.unsafe_get s (pos+6) = 'r' then (
                      0
                    )
                    else (
                      (!Ag_util.Json.unknown_field_handler) "File \"yosys_atd.atd\", line 42, characters 2-105" (String.sub s pos len); -1
                    )
                  )
                | 'm' -> (
                    if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'd' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'l' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = 's' then (
                      1
                    )
                    else (
                      (!Ag_util.Json.unknown_field_handler) "File \"yosys_atd.atd\", line 42, characters 2-105" (String.sub s pos len); -1
                    )
                  )
                | _ -> (
                    (!Ag_util.Json.unknown_field_handler) "File \"yosys_atd.atd\", line 42, characters 2-105" (String.sub s pos len); -1
                  )
            )
            else (
              (!Ag_util.Json.unknown_field_handler) "File \"yosys_atd.atd\", line 42, characters 2-105" (String.sub s pos len); -1
            )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Ag_oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_creator := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x1;
            | 1 ->
              field_modl := (
                (
                  read__8
                ) p lb
              );
              bits0 := !bits0 lor 0x2;
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x3 then Ag_oj_run.missing_fields p [| !bits0 |] [| "creator"; "modl" |];
        (
          {
            creator = !field_creator;
            modl = !field_modl;
          }
         : t)
      )
)
let t_of_string s =
  read_t (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
