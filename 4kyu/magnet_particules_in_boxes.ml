let doubles (maxk: int) (maxn: int): float =
    let rec loop acc k =
        if k > maxk then acc
        else
            let k' = float_of_int k in
            let row = List.init maxn (fun ix -> 1.0 /. (float_of_int (maxn - ix) +. 1.0) ** (2.0 *. k')) in
            let s = List.fold_left (+.) 0.0 row in
            loop (acc +. (s /. k')) (k + 1) in
    loop 0.0 1
