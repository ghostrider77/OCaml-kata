let max_ball (v0: int): int =
    let v = (float_of_int v0) /. 3.6 in
    let g = 9.81 in
    let h v t = v *. t -. 0.5 *. g *. (t ** 2.0) in
    let rec loop k t max_h =
        let height = h v t in
        if height <= max_h then (k - 1)
        else loop (k + 1) (t +. 0.1) height
    in loop 0 0.0 (-1.0)
