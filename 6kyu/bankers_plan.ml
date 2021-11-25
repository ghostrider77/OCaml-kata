let fortune (fortinit: int) (p: float) (peryear: int) (nbyear: int) (infl: float): bool =
    let rec loop f c yr =
        if f < 0 then false
        else if yr >= nbyear - 1 then true
        else
            let f = float f in
            let c = float c in
            let f' = int_of_float @@ f +. f *. (p /. 100.0) -. c in
            let c' = int_of_float @@ c +. c *. (infl /. 100.0) in
            loop f' c' (yr + 1)
    in loop fortinit peryear 0
