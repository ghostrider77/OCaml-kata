let get_average lst =
    let sum = List.fold_left (+) 0 lst in
    let n = List.length lst
    in sum / n
