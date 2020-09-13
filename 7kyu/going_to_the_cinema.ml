let movie (card: int) (ticket: int) (perc: float): int =
    let t = float_of_int ticket in
    let rec loop pa pb n =
        if ceil pa < pb then n
        else
            let pa' = pa +. t *. (perc ** (float_of_int (n + 1))) in
            let pb' = pb +. t in
            loop pa' pb' (n + 1) in
    loop (float_of_int card) 0.0 0
