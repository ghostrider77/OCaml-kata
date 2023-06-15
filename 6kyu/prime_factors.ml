let prime_factors (n : int) : int list =
    let limit = n |> float |> sqrt |> int_of_float in
    let rec loop acc p m =
        if p > limit then let acc' = if m > 1 then m :: acc else acc in List.rev acc'
        else
            let p' = if p = 2 then p + 1 else p + 2 in
            if m mod p <> 0 then loop acc p' m
            else loop (p :: acc) p (m / p) in
    loop [] 2 n
