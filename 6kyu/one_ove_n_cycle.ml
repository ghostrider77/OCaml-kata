let cycle n =
    let rec loop c m =
        let r = m mod n in
        if r = 1 then c
        else loop (c + 1) (10 * r) in
    if n mod 2 = 0 || n mod 5 = 0 then -1
    else loop 1 10
