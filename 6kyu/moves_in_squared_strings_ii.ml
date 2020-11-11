let rotate s =
    let xs = String.split_on_char '\n' s in
    let reverse x = x |> String.to_seq |> List.of_seq |> List.rev |> List.map Char.escaped |> String.concat "" in
    List.(xs |> map reverse |> rev)


let rot(s: string): string =
    let rotated = rotate s in
    String.concat "\n" rotated


let selfie_and_rot (s: string): string =
    let xs = String.split_on_char '\n' s in
    let length = List.length xs in
    let dots = String.make length '.' in
    let upper = List.map (fun row -> row ^ dots) xs in
    let rotated = rotate s in
    let lower = List.map (fun row -> dots ^ row) rotated in
    String.concat "\n" @@ upper @ lower


let oper f s = f s
