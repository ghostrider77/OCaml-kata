let remove (integer_list: int list) (values_list: int list): int list =
    List.filter (fun x -> not (List.mem x values_list)) integer_list
