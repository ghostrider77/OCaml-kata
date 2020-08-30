let parts_sums lst =
    lst |> List.rev |> List.fold_left (fun acc elem -> (List.hd acc + elem) :: acc) [0]
