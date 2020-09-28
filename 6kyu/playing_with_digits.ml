let dig_pow (n: int) (p: int): int =
    let digits = n
        |> string_of_int
        |> String.to_seq
        |> List.of_seq
        |> List.map (fun c -> int_of_string (Char.escaped c)) in
    let pow n k =
        let rec loop acc alpha =
            if alpha = k then acc
            else loop (acc * n) (alpha + 1) in
        loop 1 0 in
    let s = fst @@ List.fold_left (fun (acc, k) d -> (acc + pow d k, k + 1)) (0, p) digits in
    if s mod n = 0 then s / n else -1
