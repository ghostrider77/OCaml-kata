type parity = Even | Odd


let subsets_parity (n : int) (k : int) : parity =
  let rec aux n k =
    if k = 1 then if n mod 2 = 0 then Even else Odd
    else if n mod 2 = 1 then aux (n / 2) (k / 2)
    else if k mod 2 = 1 then Even
    else aux (n / 2) (k / 2) in
  aux n k
