let disemvowel (str : string) : string =
  str
    |> String.to_seq
    |> Seq.filter (fun chr -> not @@ String.contains "aeiou" (Char.lowercase_ascii chr))
    |> String.of_seq
