let drop_while (l: 'a list) (predicate: 'a -> bool): 'a list =
    let rec loop = function
        | x :: xs when predicate x -> loop xs
        | xs -> xs in
    loop l
