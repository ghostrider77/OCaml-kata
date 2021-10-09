let play_pass s shift =
    let circular_shift letter k =
        let shift_char code = (code - 97 + k) mod 26 + 97 in
        Char.(letter |> lowercase_ascii |> code |> shift_char |> chr) in
    let is_digit c =
        let code = Char.code c in
        code >= 48 && code <= 57 in
    let is_letter c =
        let code = Char.code c in
        (97 <= code && code <= 122) || (65 <= code && code <= 90) in
    let complement_digit c =
        let code = Char.code c in
        Char.chr (57 - code + 48) in
    let transform ix c =
        let transformed =
            if is_digit c then complement_digit c
            else if is_letter c then circular_shift c shift
            else c in
        if ix mod 2 = 0 then Char.uppercase_ascii transformed
        else Char.lowercase_ascii transformed in
    let reverse s = s
        |> String.to_seq
        |> List.of_seq
        |> List.rev
        |> List.map (String.make 1)
        |> String.concat "" in
    reverse @@ String.mapi transform s
