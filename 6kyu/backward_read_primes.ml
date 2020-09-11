let range a b s =
    let rec loop acc k =
        if k > b then List.rev acc
        else loop (k :: acc) (k + s)
    in loop [] a


let is_prime n =
    if n = 2 then true
    else if n = 1 || n mod 2 = 0 then false
    else
        let limit = n |> float_of_int |> sqrt |> floor |> int_of_float
        in List.for_all (fun k -> n mod k <> 0) (range 3 limit 2)


let is_backward_prime p =
    let reversed =
        let digits = p |> string_of_int |> String.to_seq |> List.of_seq in
        digits |> List.rev |> List.map Char.escaped |> String.concat "" |> int_of_string in
    reversed <> p && is_prime reversed


let backwardsPrime a b =
    let rec loop acc p =
        if p > b then List.rev acc
        else if is_prime p && is_backward_prime p then loop (p :: acc) (p + 1)
        else loop acc (p + 1)
    in loop [] a
