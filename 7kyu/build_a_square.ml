let generate_shape (n: int): string =
    let row = String.make n '+' in
    String.concat "\n" (List.init n (fun _ -> row))
