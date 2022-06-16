let alternate (n: int) (first_value: 'a) (second_value: 'a): 'a list =
    List.init n (fun ix -> if ix mod 2 = 0 then first_value else second_value)
