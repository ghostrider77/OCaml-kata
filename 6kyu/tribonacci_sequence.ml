exception Short_signature


let tribonacci signature n =
    let rec loop acc k = match acc with
        | a :: b :: c :: _ ->
            if k > n then List.rev acc
            else let next = a + b + c in loop (next :: acc) (succ k)
        | _ ->  raise Short_signature in
    let sequence = loop (List.rev signature) 4 in
    match sequence with
        | a :: b :: rest ->
            if n = 0 then []
             else if n = 1 then [a]
             else if n = 2 then [a; b]
             else sequence
        | _ -> raise Short_signature
