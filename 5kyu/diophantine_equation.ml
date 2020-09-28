let diophantine_solution n =
    let calc_divisor_pairs m =
        let limit = m |> float_of_int |> sqrt |> int_of_float in
        let rec loop acc k =
            if k > limit then acc
            else if m mod k = 0 then loop ((k, m / k) :: acc) (k + 1)
            else loop acc (k + 1)
        in loop [] 1 in
    let divisor_pairs = calc_divisor_pairs n in
    let valid_pairs = List.filter (fun (a, b) -> (a + b) mod 2 = 0 && (b - a) mod 4 = 0) divisor_pairs in
    let result = List.map (fun (a, b) -> ((a + b) / 2, (b - a) / 4)) valid_pairs in
    let sorted = List.sort (fun (x1, _) (x2, _) -> Stdlib.compare x2 x1) result in
    String.concat "" @@ List.map (fun (x, y) -> "(" ^ (string_of_int x) ^ ", " ^ (string_of_int y) ^ ")") sorted
