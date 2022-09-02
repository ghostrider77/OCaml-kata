let grow (xs: int list): int =
    List.fold_left ( * ) 1 xs
