let fusc (n: int): int =
    let rec loop (k: int) (a: int) (b: int) =
        if k = 0 then a
        else if k = 1 then a + b
        else if k mod 2 = 0 then loop (k / 2) a (a + b)
        else loop ((k - 1) / 2) (a + b) b
    in loop n 0 1
