let zip_with (map_func : 'a -> 'b -> 'c) (a : 'a list) (b : 'b list) : 'c list =
    let rec loop acc = function
        | (x :: xs, y :: ys) -> loop ((map_func x y) :: acc) (xs, ys)
        | _ -> List.rev acc in
    loop [] (a, b)
