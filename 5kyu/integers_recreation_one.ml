let integer_sum_of_square_divisors k =
    let limit = k / 2 in
    let rec loop acc d =
        if d > limit then acc
        else
            let acc' = if k mod d = 0 then acc + d*d else acc
            in loop acc' (d + 1) in
    let divisor_square_sum = loop (k * k) 1 in
    let is_square_number n =
        let root = n |> float_of_int |> sqrt |> int_of_float
        in root * root = n in
    if is_square_number divisor_square_sum then Some divisor_square_sum else None


let list_squared n m =
    let rec loop acc k =
        if k > m then List.rev acc
        else match integer_sum_of_square_divisors k with
            | None -> loop acc (k + 1)
            | Some s -> loop ((k, s) :: acc) (k + 1) in
    let pair_list = loop [] n in
    let string_list = List.map (fun (a, b) -> "(" ^ (string_of_int a) ^ ", "  ^ (string_of_int b) ^ ")") pair_list
    in String.concat "; " string_list
