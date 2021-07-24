let calc_sequence m =
    let u = Array.make m 1 in
    for i = 3 to m do
        u.(i - 1) <- u.(i - u.(i - 2) - 1) + u.(i - u.(i - 3) - 1);
    done;
    u

let length_sup_u_k n k =
    let sequence = calc_sequence n
    in Array.fold_left (fun acc u -> if u >= k then acc + 1 else acc) 0 sequence


let comp n =
    let sequence = calc_sequence n in
    let rec loop acc i =
        if i > n then acc
        else if sequence.(i - 1) < sequence.(i - 2) then loop (acc + 1) (i + 1)
        else loop acc (i + 1)
    in loop 0 2
