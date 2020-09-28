let f (x: float) (y: float) (eps: float): int =
    let abs_z = hypot x y in
    if abs_z >= 1.0 then -1
    else int_of_float ((log eps) /. (log abs_z))
