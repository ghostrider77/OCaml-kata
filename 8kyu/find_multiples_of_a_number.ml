let find_multiples (integer: int) (limit: int): int list =
    let n = limit / integer in
    List.init n (fun k -> (k + 1) * integer)
