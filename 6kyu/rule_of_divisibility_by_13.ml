let thirt n =
    let remainders = [1; 10; 9; 12; 3; 4] in
    let one_pass n =
        let digits = n
            |> string_of_int
            |> String.to_seq
            |> List.of_seq
            |> List.map (fun c -> int_of_string (Char.escaped c)) in
        let reversed_digits = List.rev digits in
        let rec loop acc = function
            | ([], _) -> acc
            | (d :: dss, r :: rss) -> loop (acc + d * r) (dss, rss)
            | (ds, []) -> loop acc (ds, remainders) in
        loop 0 (reversed_digits, remainders) in
    let rec loop prev =
        let next = one_pass prev in
        if prev = next then next
        else loop next
    in loop n
