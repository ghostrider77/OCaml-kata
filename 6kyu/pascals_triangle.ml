let pascals_triangle (n: int): int list =
    let get_next_row xs =
        let rec aux acc = function
            | a :: b :: rest -> aux ((a + b) :: acc) (b :: rest)
            | ([] | [_]) -> 1 :: acc
        in aux [1] xs in
    let rec aux acc row k =
        if k = n then List.concat @@ List.rev acc
        else
            let row' = get_next_row row in
            aux (row :: acc) row' (k + 1) in
    aux [] [1] 0
