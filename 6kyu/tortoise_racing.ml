let race (v1: int) (v2: int) (g: int) : int list option =
    let hms t =
        let h = floor t in
        let t = 60.0 *. (max 0.0 (t -. h)) in
        let m = if (ceil t -. t) < 1e-10 then ceil t else floor t in
        let s = floor (60.0 *. (max 0.0 (t -. m)))
        in List.map int_of_float [h; m; s] in
    if v1 >= v2 then None
    else
        let t = (float_of_int g) /. float_of_int (v2 - v1) in
        Some (hms t)
