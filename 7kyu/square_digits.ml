let square_digits (number : int) : int =
  number
    |> string_of_int
    |> String.to_seq
    |> Seq.map (fun chr -> let digit = int_of_string (String.make 1 chr) in string_of_int (digit * digit))
    |> List.of_seq
    |> String.concat ""
    |> int_of_string
