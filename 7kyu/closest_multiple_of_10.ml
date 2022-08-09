let closest_multiple_10 (i: int): int =
    10 * (int_of_float @@ floor (float i /. 10.0 +. 0.5))
