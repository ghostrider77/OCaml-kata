let lovefunc (flower1: int) (flower2: int): bool =
    (flower1 mod 2 = 0 && flower2 mod 2 = 1) || (flower1 mod 2 = 1 && flower2 mod 2 = 0)
