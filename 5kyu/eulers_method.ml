let ex_euler n =
    let f x y = 2.0 -. exp(-4.0 *. x) -. 2.0 *. y in
    let g x = 1.0 +. 0.5 *. (exp(-4.0 *. x) -. exp(-2.0 *. x)) in
    let h = 1.0 /. float_of_int n in
    let truncate r =  floor (r *. 1000000.0) /. 1000000.0 in
    let xs = List.init n (fun ix -> (float_of_int (ix + 1)) *. h) in
    let process (errors, y_prev) x_new =
        let x_prev = x_new -. h in
        let y_new = y_prev +. h *. (f x_prev y_prev) in
        let z_new = g x_new in
        let rel_err = abs_float (y_new -. z_new) /. z_new in
        (rel_err :: errors, y_new) in
    let relative_errors = fst @@ List.fold_left process ([0.0], 1.0) xs in
    let mean_error = (List.fold_left (+.) 0.0 relative_errors) /. float_of_int (n + 1)
    in truncate mean_error
