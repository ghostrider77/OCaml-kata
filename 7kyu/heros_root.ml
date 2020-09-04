let int_rac (n : int) (guess : int) =
    let rec loop c x =
        let x' = (x + n / x) / 2 in
        if abs (x' - x) < 1 then c
        else loop (c + 1) x'
    in loop 1 guess
