let find_ball scales =
    let find_heavier a b c =
        let w = scales#get_weight [a] [b] in
        if w < 0 then a
        else if w > 0 then b
        else c in
    let w = scales#get_weight [0; 1; 2] [3; 4; 5] in
    if w = 0 then if scales#get_weight [6] [7] < 0 then 6 else 7
    else if w < 0 then find_heavier 0 1 2
    else find_heavier 3 4 5
