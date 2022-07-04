let take_while (l: 'a list) (predicate: 'a -> bool): 'a list =
    let rec loop acc = function
        | x :: xs when predicate x -> loop (x :: acc) xs
        | _ -> List.rev acc in
    loop [] l
