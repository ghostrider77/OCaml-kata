let digital_root (n : int) : int =
    let to_digits k =
        string_of_int k |> String.to_seq |> List.of_seq |> List.map (fun c -> int_of_string (Char.escaped c)) in
    let rec loop root =
        if root < 10 then root
        else
            let digits = to_digits root in
            loop (List.fold_left (+) 0 digits)
    in loop n
