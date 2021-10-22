let tankvol (h: int) (d: int) (vt: int): int =
    let r = float d /. 2.0 in
    let length = float vt /. (Float.pi *. r ** 2.0) in
    let beta = acos ((r -. float h) /. r) in
    let half_arc = r *. beta in
    let area = half_arc *. r -. (r *. r *. sin (2.0 *. beta)) /. 2.0 in
    int_of_float (length *. area)
