let find_missing_number (xs : int list) : int =
  let n = List.length xs + 1 in
  let sum = List.fold_left (+) 0 xs in
  n * (n + 1) / 2 - sum
