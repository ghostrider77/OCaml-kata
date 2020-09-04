let seven(m: int): int * int =
    let rec loop n steps =
        if n < 100 then (n, steps)
        else
            let k = n / 10 in
            let r = n - k *10
            in loop (k - 2*r) (steps + 1)
    in loop m 0
