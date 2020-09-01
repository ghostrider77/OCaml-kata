let part_array arr =
    let rec loop acc left = function
        | ([] | [_]) -> acc |> List.rev |> Array.of_list
        | x :: xs ->
            let left' = List.rev (x :: List.rev left) in
            loop ((String.concat " " left', String.concat " " xs) :: acc) left' xs
    in loop [] [] (Array.to_list arr)
