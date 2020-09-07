let epidemic (tm: int) (n: int) (s0: float) (i0: float) (b: float) (a: float) =
    let dt = (float_of_int tm) /. (float_of_int n) in
    let rec loop s i r k max_i =
        if k = n then max_i |> int_of_float
        else
            let s' = s *.(1.0 -. dt *. b *. i) in
            let i' = i +. dt *. (b *. s *. i -. a *. i) in
            let r' = r +. dt *. i *. a in
            loop s' i' r' (k + 1) (max max_i i')
    in loop s0 i0 0.0 0 min_float
