let rotate_left lists =
    let rec loop acc xs =
        if List.for_all (fun x -> x = []) xs then acc
        else let row = List.map List.hd xs in loop (row :: acc) (List.map List.tl xs)
    in loop [] lists


let snail xs =
    let rec loop acc = function
        | [] -> acc
        | row :: rest -> loop (List.append acc row) (rotate_left rest)
    in loop [] xs
