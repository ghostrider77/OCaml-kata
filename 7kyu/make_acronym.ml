let to_acronym (inp: string): string =
    inp |> String.split_on_char ' '
        |> List.map (fun s -> String.make 1 @@ Char.uppercase_ascii @@ String.get s 0)
        |> String.concat ""
