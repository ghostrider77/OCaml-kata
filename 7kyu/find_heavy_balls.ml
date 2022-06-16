let find_ball scales =
    let w1 = if scales#get_weight [1; 3; 5; 7] [0; 2; 4; 6] < 0 then 1 else 0 in
    let w2 = if scales#get_weight [2; 3; 6; 7] [0; 1; 4; 5] < 0 then 2 else 0 in
    let w3 = if scales#get_weight [4; 5; 6; 7] [0; 1; 2; 3] < 0 then 4 else 0 in
    w1 + w2 + w3
