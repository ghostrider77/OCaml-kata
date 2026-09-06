let create_phone_number (ns : int list) : string =
  match ns with
    | [a1; a2; a3; b1; b2; b3; c1; c2; c3; c4] ->
        Printf.sprintf "(%d%d%d) %d%d%d-%d%d%d%d" a1 a2 a3 b1 b2 b3 c1 c2 c3 c4
    | _ -> failwith "Unexpected input length."
