let going n =
    let rec loop acc k =
        if k = n then acc
        else
            let k = k + 1 in
            loop (acc /. (float_of_int k) +. 1.0) k in
    let fraction = loop 1.0 1 in
    floor (fraction *. 1000000.0) /. 1000000.0
