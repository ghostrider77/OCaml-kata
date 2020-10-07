let choose_best_sum (t: int) (k: int) (ls: int list): int =
    let rec subsequences = function
        | [] -> [[]]
        | x :: xs ->
            let ys = subsequences xs in
            List.append ys (List.map (fun lst -> x :: lst) ys) in
    let combinations lst k =
        let subseqs = subsequences lst in
        List.filter (fun xs -> List.length xs = k) subseqs in
    if List.length ls < k then -1
    else
        let xs = combinations ls k in
        let distances = List.(xs |> map (fold_left (+) 0) |> filter (fun d -> d <= t)) in
        match List.sort (fun x y -> Stdlib.compare y x) distances with
            | [] -> -1
            | x :: _ -> x
