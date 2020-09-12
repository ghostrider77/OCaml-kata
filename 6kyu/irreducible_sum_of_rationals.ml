let sum_fracts (xs: (int * int) list) : string option =
    let rec calc_gcd a b =
        if b = 0 then a else calc_gcd b (a mod b) in
    let calc_lcm a b =
        let gcd = calc_gcd a b in
        (a / gcd) * b in
    let simplify (p, q) =
        let g = calc_gcd p q in (p / g, q / g) in
    let add (p1, q1) (p2, q2) =
        let d = calc_lcm q1 q2 in
        simplify (d / q1 * p1 + d / q2 * p2, d) in
    match xs with
        | [] -> None
        | x :: xss ->
            let (p, q) = List.fold_left add x xss in
            if q = 1 then Some (string_of_int p)
            else Some ((string_of_int p) ^ " " ^ (string_of_int q))
