module CharSet = Set.Make(Char)


let longest s1 s2 =
    let s = s1 ^ s2 in
    let set = s |> String.to_seq |> CharSet.of_seq in
    let unique_chars = CharSet.elements set in
    unique_chars |> List.map Char.escaped |> String.concat ""
