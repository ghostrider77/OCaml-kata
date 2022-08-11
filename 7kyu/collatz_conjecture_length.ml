let collatz (n: int): int =
    let rec loop length k =
        if k = 1 then length
        else if k mod 2 = 0 then loop (length + 1) (k / 2)
        else loop (length + 1) (3*k + 1)
    in loop 1 n
