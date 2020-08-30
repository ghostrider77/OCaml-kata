let product_fib (n: int): int * int * int =
    let rec loop a b =
        let prod = a * b in
        if prod = n then (a, b, 1)
        else if prod > n then (a, b, 0)
        else loop b (a + b)
    in loop 0 1
