let extract_rows s =
    let strings = String.split_on_char '\n' s in
    List.map (fun str -> str |> String.to_seq |> List.of_seq) strings


let join chars = chars |> List.map Char.escaped |> String.concat ""


let diag_1_sym s =
    let rows = extract_rows s in
    let rec loop acc xs =
        if List.for_all (fun x -> x = []) xs then List.rev acc
        else let column = List.map List.hd xs in loop (column :: acc) (List.map List.tl xs) in
    let transpose = loop [] rows in
    transpose |> List.map join |> String.concat "\n"


let rot_90_clock s =
    let rows = extract_rows s in
    let rec loop acc xs =
        if List.for_all (fun x -> x = []) xs then List.rev acc
        else let column = List.map List.hd xs in loop (List.rev column :: acc) (List.map List.tl xs) in
    let rotated = loop [] rows in
    rotated |> List.map join |> String.concat "\n"


let selfie_and_diag1 s =
    let diag = diag_1_sym s in
    let s1 = String.split_on_char '\n' s in
    let s2 = String.split_on_char '\n' diag in
    List.map2 (fun a b -> a ^ "|" ^ b) s1 s2 |> String.concat "\n"


let oper f s = f s
