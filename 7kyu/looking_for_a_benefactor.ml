let new_avg (xs: float list) (navg: float): int option =
    let n = List.length xs in
    let s = List.fold_left (+.) 0.0 xs in
    let result = int_of_float @@ ceil ((float_of_int n +. 1.0) *. navg -. s) in
    if result <= 0 then None
    else Some result
