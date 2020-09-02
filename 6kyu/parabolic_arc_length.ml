let len_curve (n: int) =
    let f x = x *. x in
    let delta = 1.0 /. (float_of_int n) in
    let ys = List.init (n + 1) (fun ix -> f ((float_of_int ix) *. delta)) in
    let arc_length y1 y2 = hypot delta (y2 -. y1) in
    fst @@ List.fold_left
        (fun (acc, y_prev) y_now -> (acc +. (arc_length y_prev y_now), y_now))
        (0.0, List.hd ys)
        (List.tl ys)
