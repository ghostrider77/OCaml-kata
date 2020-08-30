let is_palindrome (s: string): bool =
    let lower = String.lowercase_ascii s in
    let lst = lower |> String.to_seq |> List.of_seq in
    lst = List.rev lst
