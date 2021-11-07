type point = { x : float; y : float}


let solve(a: int) (b: int) (c: int) (alpha: int) (beta: int) (gamma: int) =
    let round f = floor (f +. 0.5) in
    let rad_of_deg angle = 2.0 *. Float.pi *. float angle /. 360.0 in
    let deg_of_rad radian = radian *. 360.0 /. (2.0 *. Float.pi) in
    let represent_angle angle =
        let (fr, deg) = modf angle in
        let (fr, m) = modf (fr *. 60.0) in
        let (_, s) = modf (fr *. 60.0) in
        List.map int_of_float [deg; m; s] in
    let alpha = rad_of_deg alpha in
    let beta = rad_of_deg beta in
    let gamma = rad_of_deg gamma in
    let point_a = { x = float a *. (cos alpha); y = float a *. (sin alpha) } in
    let point_b = { x = point_a.x -. float b *. (sin beta); y = point_a.y +. float b *. (cos beta) } in
    let point_c = { x = point_b.x -. float c *. (cos gamma); y = point_b.y -. float c *. (sin gamma)} in
    let distance = int_of_float @@ round @@ Float.hypot point_c.x point_c.y in
    let get_angle p =
        let deg = deg_of_rad @@ Float.atan2 p.y p.x
        in abs_float deg in
    let angle = get_angle point_c in
    [distance] @ represent_angle angle
