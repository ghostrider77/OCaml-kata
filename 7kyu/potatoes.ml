let potatoes (p0: int) (w0: int) (p1: int): int =
    (float_of_int w0) *. (100.0 -. (float_of_int p0)) /. (100.0 -. (float_of_int p1)) |> floor |> int_of_float
