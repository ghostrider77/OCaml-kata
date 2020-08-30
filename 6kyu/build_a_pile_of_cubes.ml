open Num


let find_nb(s: string): int =
    let m = num_of_string s in
    let r = m |> float_of_num |> sqrt in
    let n = (-1.0 +. sqrt (1.0 +. 8.0 *. r)) /. 2.0 |> int_of_float in
    let k = n * (n + 1) / 2 in
    if (num_of_int k) */ (num_of_int k) =/ m then n else -1
