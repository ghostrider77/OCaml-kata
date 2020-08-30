let quadratic(a: float) (b: float) (c: float): float =
    (-2.0 *. c) /. (sqrt (b *. b -. 4.0 *. a *. c) +. b)
