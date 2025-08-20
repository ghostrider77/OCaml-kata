let generate_pairs (n : int) : (int * int) list =
  let xs = List.init (n + 1) Fun.id in
  List.concat_map (fun i -> List.map (fun j -> (i, j)) @@ List.init (n - i + 1) Fun.id) xs
