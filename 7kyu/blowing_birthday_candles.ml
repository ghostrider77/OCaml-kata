let blow_candles (s : string) : int =
  let str =
    s |> String.to_seq
      |> Seq.map (fun c -> int_of_string (String.make 1 c))
      |> Seq.drop_while (fun c -> c = 0)
      |> List.of_seq in
  let rec loop acc = function
    | [] -> acc
    | [a] -> acc + a
    | [a; b] -> acc + max a b
    | a :: b :: c :: rest ->
        if a = 0 then loop acc (b :: c :: rest)
        else loop (acc + a) ((max (b - a) 0) :: (max (c - a) 0) :: rest) in
  loop 0 str
