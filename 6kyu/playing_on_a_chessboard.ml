let game(n: int): string =
    if n mod 2 = 0 then "[" ^ string_of_int (n * (n / 2)) ^ "]"
    else "[" ^ string_of_int (n * n) ^ "," ^ string_of_int 2 ^ "]"
