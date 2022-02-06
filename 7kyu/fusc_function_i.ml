let rec fusc (n: int): int =
    if n = 0 then 0
    else if n = 1 then 1
    else if n mod 2 = 0 then fusc (n / 2)
    else fusc (n / 2) + fusc (n / 2 + 1)
