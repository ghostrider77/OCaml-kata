let crossover ns xs ys =
    let indices = List.sort_uniq compare ns in
    let n = List.length xs in
    let swaps =
        let rec loop acc current ix =
            if ix = n then List.rev acc
            else if not (List.mem ix indices) then loop (current :: acc) current (ix + 1)
            else
                let next = (-1) * current in
                loop (next :: acc) next (ix + 1) in
        loop [] 1 0 in
    let chromosomes = List.combine xs ys in
    List.split @@ List.map2 (fun swap (x, y) -> if swap = 1 then (x, y) else (y, x)) swaps chromosomes
