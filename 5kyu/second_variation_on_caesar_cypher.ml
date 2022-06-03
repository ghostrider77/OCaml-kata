let is_lowercase_letter chr =
    let code = Char.code chr in
    97 <= code && code <= 122


let shift_lowercase_letter shift chr =
    let code = Char.code chr in
    let s = (code - 97 + shift) mod 26 in
    let code' = if s >= 0 then s + 97 else s + 26 + 97 in
    Char.chr code'


let shift_uppercase_letter shift chr =
    chr |> Char.lowercase_ascii |> shift_lowercase_letter shift |> Char.uppercase_ascii


let shift_letter k chr =
    if is_lowercase_letter chr then shift_lowercase_letter k chr
    else if is_lowercase_letter (Char.lowercase_ascii chr) then shift_uppercase_letter k chr
    else chr


let create_list chars =
    let n = List.length chars in
    let k = if n mod 5 = 0 then n / 5 else n / 5 + 1 in
    let rec loop acc = function
        | [] -> List.rev_map (function lst -> lst |> List.map (String.make 1) |> String.concat "") acc
        | cs ->
            let open Batteries in
            let first = List.take k cs in
            let rest = List.drop k cs in
            loop (first :: acc) rest in
    loop [] chars


let encode (s: string) (shift: int): string list =
    match s |> String.to_seq |> List.of_seq with
        | [] -> [""]
        | (c :: _) as char_list ->
            let encoded = List.map (shift_letter shift) char_list in
            let lower = Char.lowercase_ascii c in
            create_list (lower :: (shift_letter shift lower) :: encoded)


let decode (a: string list): string =
    match a |> String.concat "" |> String.to_seq |> List.of_seq with
        | c :: c' :: rest ->
            let s = (Char.code c' - Char.code c) mod 26 in
            let shift = if s < 0 then s + 26 else s in
            rest |> List.map (shift_letter (-shift)) |> List.map (String.make 1) |> String.concat ""
        | _ -> ""
