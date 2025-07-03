let solve (xs: int list): int list =
  let rec loop acc current_max = function
    | [] -> acc
    | x :: xs ->
        if x > current_max then loop (x :: acc) x xs
        else loop acc current_max xs in
  loop [] min_int (List.rev xs)
