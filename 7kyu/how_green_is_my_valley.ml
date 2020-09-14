let make_valley (a: int list): int list =
    let sorted = List.sort (fun a b -> compare b a) a in
    let rec loop xs ys = function
        | [] -> List.rev xs, ys
        | [y] ->loop xs (y :: ys) []
        | x :: y :: rest -> loop (x :: xs) (y :: ys) rest in
    let left, right = loop [] [] sorted in
    left @ right
