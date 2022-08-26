let digitize (n: int): int list =
    n |> string_of_int
      |> String.to_seq
      |> Seq.map (fun c -> c |> String.make 1 |> int_of_string)
      |> List.of_seq
      |> List.rev
