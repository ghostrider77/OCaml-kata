let extract_rows s =
    let strings = String.split_on_char '\n' s in
    List.map (fun str -> str |> String.to_seq |> List.of_seq) strings


let join chars = chars |> List.map Char.escaped |> String.concat ""


let rot_90_counter s =
    let rows = s |> extract_rows |> List.map List.rev in
    let rec loop acc xs =
        if List.for_all (fun x -> x = []) xs then List.rev acc
        else let column = List.map List.hd xs in loop (column :: acc) (List.map List.tl xs) in
    let transpose = loop [] rows in
    transpose |> List.map join |> String.concat "\n"


let diag_2_sym s =
    let rows = s |> extract_rows |> List.map List.rev |> List.rev in
    let rec loop acc xs =
        if List.for_all (fun x -> x = []) xs then List.rev acc
        else let column = List.map List.hd xs in loop (column :: acc) (List.map List.tl xs) in
    let transpose = loop [] rows in
    transpose |> List.map join |> String.concat "\n"


let selfie_diag2_counterclock s =
    let combine lst1 lst2 =
        List.map2 (fun a b -> a ^ "|" ^ b) lst1 lst2 in
    let orig = s |> extract_rows |> List.map join in
    let s1 = s |> diag_2_sym |> extract_rows |> List.map join in
    let s2 = s |> rot_90_counter |> extract_rows |> List.map join in
    String.concat "\n" @@ combine (combine orig s1) s2


let oper f s = f s
