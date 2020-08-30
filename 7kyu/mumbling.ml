let accum str =
    let lst = str |> String.to_seq |> List.of_seq in
    let stringlist =
        List.mapi (fun ix chr -> String.(chr |> make (ix + 1) |> lowercase_ascii |> capitalize_ascii)) lst
    in String.concat "-" stringlist
