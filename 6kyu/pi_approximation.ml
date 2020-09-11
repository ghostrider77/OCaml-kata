let iter_pi eps =
    let truncate r = (r *. 1e10 |> int_of_float |> float_of_int) /. 1e10 in
    let rec loop y n =
        if abs_float (y *. 4.0 -. Float.pi) < eps then (n, truncate (4.0 *. y))
        else
            let sign = if n mod 2 = 0 then 1.0 else (-1.0) in
            let y_new = y +. sign /. (2.0 *. (float_of_int n) +. 1.0) in
            loop y_new (n + 1)
    in loop 0.0 0
