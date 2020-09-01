let nb_year (p0: int) (percent: float) (aug: int) (p: int): int =
    let p0 = float_of_int p0 in
    let p = float_of_int p in
    let aug = float_of_int aug in
    let rec loop pn yrs =
        if pn >= p then yrs
        else
            let p_next = floor (pn *. (1.0 +. percent /. 100.0) +. aug)
            in loop p_next (yrs + 1)
    in loop p0 0
