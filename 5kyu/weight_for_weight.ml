let order_weight(s: string): string =
    let sum_of_digits n =
        n |> String.to_seq
          |> List.of_seq
          |> List.map (fun c -> int_of_string (Char.escaped c))
          |> List.fold_left (+) 0 in
    let compare s1 s2 =
        let result = Stdlib.compare (sum_of_digits s1) (sum_of_digits s2) in
        if result <> 0 then result
        else Stdlib.compare s1 s2 in
    let number_strings = String.split_on_char ' ' s in
    String.concat " " @@ List.sort compare number_strings
