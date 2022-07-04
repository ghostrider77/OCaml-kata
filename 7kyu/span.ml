let span (l: 'a list) (predicate: 'a -> bool): 'a list * 'a list =
    let rec loop acc = function
        | x :: xs when predicate x -> loop (x :: acc) xs
        | xs -> List.rev acc, xs in
    loop [] l
