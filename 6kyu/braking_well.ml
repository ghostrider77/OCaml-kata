let g = 9.81


let dist(v: float) (mu: float): float =
    let v = v /. 3.6 in
    v +. (v *. v) /. (2.0 *. g *. mu)


let speed(d: float) (mu: float): float =
    let gmu = g *. mu in
    let v = sqrt (gmu *. (gmu +. 2.0 *. d)) -. gmu in
    v *. 3.6
