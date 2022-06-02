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
        | [] -> List.rev_map (fun lst -> lst |> List.map (String.make 1) |> String.concat "") acc
        | cs ->
            let open Batteries in
            let first = List.take k cs in
            let rest = List.drop k cs in
            loop (first :: acc) rest in
    let parts = loop [] chars in
    if List.length parts < 5 then parts @ [""] else parts


let moving_shift (s: string) (shift: int) =
    let rec loop acc k = function
        | [] -> List.rev acc
        | c :: cs ->
            let shifted_char = shift_letter k c in
            loop (shifted_char :: acc) (k + 1) cs in
    let char_list = s |> String.to_seq |> List.of_seq in
    char_list |> loop [] shift |> create_list


let demoving_shift (a: string list) (shift: int): string =
    let shifted = a |> String.concat "" |> String.to_seq |> List.of_seq in
    let rec loop acc k = function
        | [] -> acc |> List.rev_map (String.make 1) |> String.concat ""
        | c :: cs -> loop ((shift_letter (-k) c) :: acc) (k + 1) cs in
    loop [] shift shifted
