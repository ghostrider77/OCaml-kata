let nb_dig(n: int) (d: int): int =
    let d = char_of_int (d + 48) in
    let squares = List.init (n + 1) (fun ix -> string_of_int (ix * ix)) in
    let digits = String.(squares |> concat "" |> to_seq) in
    Seq.fold_left (fun count c -> if c = d then count + 1 else count) 0 digits
