type number = { n : int; ix : int; weight : int }


let closest (strng: string): int array * int array =
    let calc_weight n =
        let seq = String.to_seq n in
        Seq.(seq |> map (fun d -> int_of_string @@ String.make 1 d) |> fold_left (+) 0) in
    let calc_pairs xs =
        List.(xs |> map (fun x1 -> List.map (fun x2 -> (x1, x2)) xs) |> concat) in
    let ns = String.split_on_char ' ' strng in
    let xs = List.mapi (fun ix n -> let w = calc_weight n in {n = int_of_string n; ix; weight = w}) ns in
    let pairs = xs |> calc_pairs |> List.filter (fun ({ix = i}, {ix = j}) -> i < j) in
    let process (((item1, item2), smallest_diff, smallest_weight, smallest_ix) as acc) (n1, n2) =
        let diff = abs (n1.weight - n2.weight) in
        let s_w = min n1.weight n2.weight in
        if (diff < smallest_diff) ||
            (diff = smallest_diff && s_w < smallest_weight) ||
            (diff = smallest_weight && s_w = smallest_weight && n1.ix < smallest_ix)
        then ((n1, n2), diff, s_w, n1.ix)
        else acc in
    let (a, b) = List.hd pairs in
    let initial_diff = abs (a.weight - b.weight) in
    let initial_weight = min a.weight b.weight in
    let ((x, y), _, _, _) = List.fold_left process ((a, b), initial_diff, initial_weight, 0) pairs in
    let (x, y) = if (x.weight < y.weight) || (x.weight = y.weight && x.ix < y.ix) then (x, y) else (y, x)
    in [|x.weight; x.ix; x.n|], [|y.weight; y.ix; y.n|]
