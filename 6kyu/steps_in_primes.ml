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


let step g m n =
    let rec loop a =
        let b = a + g in
        if b > n then []
        else if is_prime a && is_prime b then [a; b]
        else loop (a + 1)
    in loop m
