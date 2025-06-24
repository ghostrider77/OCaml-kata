let powers (n : int) : int list =
  let rec loop acc sum p k =
    if sum = n then List.rev acc
    else
      let digit = (n lsr k) land 1 in
      if digit = 0 then loop acc sum (2*p) (k + 1)
      else loop (p :: acc) (sum + p) (2*p) (k + 1) in
  loop [] 0 1 0
