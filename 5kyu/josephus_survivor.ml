let josephus_survivor (n: int) (k: int): int =
    let rec loop acc m =
        if m > n then acc
        else loop ((k - 1 + acc) mod m + 1) (m + 1)
    in loop 1 1
