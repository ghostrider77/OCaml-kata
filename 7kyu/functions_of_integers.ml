let sumin (n: int): int =
    let rec loop acc i =
        if i > n then acc
        else loop (acc + i * (2*n - i + 1) / 2) (i + 1)
    in loop 0 1

let sumsum (n: int): int = n * n * (n + 1)

let sumax (n: int): int = sumsum n - sumin n
