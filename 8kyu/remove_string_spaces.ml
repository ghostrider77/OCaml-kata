let no_space (str : string) : string =
  str
    |> String.to_seq
    |> Seq.filter (fun c -> c <> ' ')
    |> String.of_seq
