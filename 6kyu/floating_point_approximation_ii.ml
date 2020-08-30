let interp f l u n =
    let d = (u -. l) /. (float_of_int n) in
    let fl y = floor (y *. 100.0) /. 100.0
    in List.init n (fun k -> let p = (float_of_int k) *. d in fl @@ f p)
