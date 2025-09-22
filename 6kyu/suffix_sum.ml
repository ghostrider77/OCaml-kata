let suffix_sums (xs : int list) : int list =
  xs
    |> List.rev
    |> List.to_seq
    |> Seq.scan (+) 0
    |> Seq.drop 1
    |> List.of_seq
    |> List.rev
