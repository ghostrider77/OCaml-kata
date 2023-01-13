let transpose_matrix (matrix : 'a list list) : 'a list list =
    let rec loop acc xs =
        if List.for_all (fun x -> x = []) xs then List.rev acc
        else
            let row = List.(xs |> map hd) in
            loop (row :: acc) (List.(xs |> map tl))
    in loop [] matrix
