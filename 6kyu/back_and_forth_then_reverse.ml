let arrange (s: 'a list): 'a list =
    let n = List.length s in
    let is_even = n mod 2 = 0 in
    let rec loop acc k = function
        | (x :: xs, y :: ys) ->
            if k >= n / 2 then if is_even then List.rev acc else List.rev (x :: acc) else
            let acc' = if k mod 2 = 1 then x :: y :: acc else y :: x :: acc in
            loop acc' (k + 1) (xs, ys)
        | _ -> List.rev acc in
    loop [] 0 (s, List.rev s)
