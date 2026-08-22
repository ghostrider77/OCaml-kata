let solution (n : int) : int =
  let sum_of_divisor_multiples d =
    let limit = max 0 (n - 1) in
    let k = limit / d in
    k * (k + 1) * d / 2 in
  if n < 0 then 0
  else sum_of_divisor_multiples 3 + sum_of_divisor_multiples 5 - sum_of_divisor_multiples 15
