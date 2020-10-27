let simpson (n: int) =
    let f x = let s = sin x in 1.5 *. s *. s *. s in
    let h = Float.pi /. (float_of_int n) in
    let xs = List.init (n + 1) (fun k -> (float_of_int k) *. h) in
    let ys = List.map f xs in
    let ws = List.init (n + 1) (fun k -> if k = 0 || k = n then 1 else if k mod 2 = 1 then 4 else 2) in
    (List.fold_left2 (fun acc w y -> acc +. (float_of_int w) *. y) 0.0 ws ys) *. h /. 3.0
