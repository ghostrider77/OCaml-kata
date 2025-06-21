let solve (xs: int list): int =
  let sorted = List.sort (fun x y -> compare (abs x) (abs y)) xs in
  let rec loop = function
    | a :: b :: rest ->
        if a + b = 0 then loop rest
        else a
    | [a] -> a
    | _ -> failwith "No solution found." in
  loop sorted
