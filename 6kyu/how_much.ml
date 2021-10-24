let howmuch (m: int) (n: int) =
    let (m, n) = if (m <= n) then (m, n) else (n, m) in
    let modulus = 63 in
    let start =
        let k = m / modulus in
        let m' = k * modulus + 37 in
        if m' < m then m' + modulus else m' in
    let rec loop acc money =
        if money > n then String.concat " / " (List.rev acc)
        else
            let car = (money - 1) / 9 in
            let boat = (money - 2) / 7 in
            let record = ["M: " ^ string_of_int money; "B: " ^ string_of_int boat; "C " ^ string_of_int car] in
        loop ((String.concat " ; " record) :: acc) (money + modulus)
    in loop [] start
