let race_podium (blocks: int): int * int * int =
    let first = int_of_float @@ ceil @@ float (blocks + 3) /. 3.0 in
    let rest = blocks - first in
    let second = (min first rest) - 1 in
    let third = blocks - (first + second)
    in (second, first, third)
