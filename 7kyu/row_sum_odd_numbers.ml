let row_sum_odd_numbers (n: int): int =
    let ix = (n - 1) * n / 2 in
    let rec loop acc k c =
        if c >= n then acc
        else loop (acc + k) (k + 2) (c + 1)
    in loop 0 (2*ix + 1) 0
