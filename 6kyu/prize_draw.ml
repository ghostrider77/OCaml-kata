let rank (st: string) (we: int array) (n: int): string =
    if st = "" then "No participants"
    else
        let calc_rank name =
            Seq.fold_left (fun acc c -> acc + (Char.(c |> lowercase_ascii |> code) - 96))
            (String.length name) (String.to_seq name) in
        let names = String.split_on_char ',' st in
        let nr_names = List.length names in
        let weights = Array.to_list @@ Array.sub we 0 nr_names in
        let winning_numbers = List.map2 (fun name w -> (w * (calc_rank name), name)) names weights in
        let compare (w1, n1) (w2, n2) =
            if w1 > w2 then 1
            else if w1 < w2 then -1
            else if n1 > n2 then -1
            else if n1 < n2 then 1
            else 0 in
        let sorted_names = List.rev @@ List.sort compare winning_numbers in
        if n > List.length sorted_names then "Not enough participants"
        else snd @@ List.nth sorted_names (n - 1)
