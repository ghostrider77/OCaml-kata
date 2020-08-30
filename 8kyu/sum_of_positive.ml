let positive_sum ls =
    List.fold_left (fun acc elem -> if elem > 0 then acc + elem else acc) 0 ls
