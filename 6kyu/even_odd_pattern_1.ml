let even_odd (xs : int list) : int =
  xs |> List.to_seq |> Seq.fold_lefti (fun acc ix x -> if ix mod 2 = 0 then acc + x else acc * x) 0
