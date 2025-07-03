let interlockable (a: int64) (b: int64): bool =
  let rec loop a b =
    if a = 0L || b = 0L then true
    else if (Int64.rem a 2L = 1L) && (Int64.rem b 2L = 1L) then false
    else loop (Int64.div a 2L) (Int64.div b 2L) in
  loop a b
