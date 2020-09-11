let convert_fracts (ls: (int * int) list): (int * int) list =
    let rec calc_gcd a b =
        if b = 0 then a else calc_gcd b (a mod b) in
    let calc_lcm a b =
        let gcd = calc_gcd a b in
        (a / gcd) * b in
    let simplify (p, q) =
        let g = calc_gcd p q in (p / g, q / g) in
    let reduced_fractions = List.map simplify ls in
    let denominators = snd @@ List.split reduced_fractions in
    let d = List.fold_left calc_lcm 1 denominators in
    List.map (fun (p, q) -> ((d / q) * p, d)) reduced_fractions
