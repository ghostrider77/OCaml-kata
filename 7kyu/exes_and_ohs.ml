let xo (s: string): bool =
    let count item =
        s |> String.to_seq
          |> Seq.fold_left (fun acc c -> if Char.lowercase_ascii c = item then acc + 1 else acc) 0 in
    count 'x' = count 'o'
