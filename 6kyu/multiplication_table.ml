let multiplication_table size =
    List.init size (fun i -> List.init size (fun j -> (i + 1) * (j + 1)))
