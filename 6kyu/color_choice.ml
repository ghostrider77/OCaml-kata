let checkchoose (m: int) (n: int): int =
    let rec loop acc k =
        if acc = m then k
        else if k = n then -1
        else loop (acc * (n - k) / (k + 1)) (k + 1)
    in if m = 1 then 0 else loop n 1
