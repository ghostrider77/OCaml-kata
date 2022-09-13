let find_next_power (val_: int) (pow: int): int =
    let n = ceil @@ (float val_) ** (1.0 /. float pow) in
    int_of_float @@ n ** (float pow)
