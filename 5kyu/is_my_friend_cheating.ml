let remov_nb (n: int): string =
    let stringify result =
        String.concat "" (List.map (fun (a, b) -> "(" ^ string_of_int a ^ ", " ^ string_of_int b ^ ")") result) in
    let s = n * (n + 1) / 2 + 1 in
    let rec loop acc k =
        if k = n + 2 then stringify @@ List.rev acc
        else if s mod k <> 0 then loop acc (k + 1)
        else
            let m = s / k in
            if m >= 2 && m < n + 1 && m <> k then loop ((k - 1, m - 1) :: acc) (k + 1)
            else loop acc (k + 1)
    in loop [] 2
