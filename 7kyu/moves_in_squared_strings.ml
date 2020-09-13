let vert_mirror (s: string): string =
    let xs = String.split_on_char '\n' s in
    let reverse x = x |> String.to_seq |> List.of_seq |> List.rev |> List.map Char.escaped |> String.concat "" in
    xs |> List.map reverse |> String.concat "\n"


let hor_mirror (s: string): string =
    let xs = String.split_on_char '\n' s in
    xs |> List.rev |> String.concat "\n"


let oper f s = f s
