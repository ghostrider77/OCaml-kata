let mxdiflg (a1: string array) (a2: string array): int option =
    if Array.length a1 = 0 || Array.length a2 = 0 then None
    else
        let xs1 = Array.map String.length a1 in
        let xs2 = Array.map String.length a2 in
        Some (Array.fold_left
              (fun acc l1 -> Array.fold_left (fun inner l2 -> max inner (abs (l1 - l2))) acc xs2)
              min_int xs1)
