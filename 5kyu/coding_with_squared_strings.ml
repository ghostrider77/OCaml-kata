let extract_rows s =
    let strings = String.split_on_char '\n' s in
    List.map (fun str -> str |> String.to_seq |> List.of_seq) strings


let join chars = chars |> List.map (String.make 1) |> String.concat ""


let code (t: string): string =
    let add_padding s =
        let length = String.length t in
        let n = length |> float |> sqrt |> ceil |> int_of_float in
        let d = n * n - length in
        if d = 0 then s else s ^ String.make d '&' in
    let add_newline s =
        let length = String.length s in
        let n = length |> float |> sqrt |> int_of_float in
        let chars = s |> String.to_seq |> List.of_seq in
        let rec loop acc ix = function
            | [] -> String.concat "" @@ List.map (String.make 1) (List.rev acc)
            | c :: cs ->
                if ix mod n = 0 && ix <> 0 then loop (c :: '\n' :: acc) (ix + 1) cs
                else loop (c :: acc) (ix + 1) cs
        in loop [] 0 chars in
    let rot_90_clock s =
        let rows = extract_rows s in
        let rec loop acc xs =
            if List.for_all (fun x -> x = []) xs then List.rev acc
            else let column = List.map List.hd xs in loop (List.rev column :: acc) (List.map List.tl xs) in
        let rotated = loop [] rows in
        rotated |> List.map join |> String.concat "\n"
    in t |> add_padding |> add_newline |> rot_90_clock


let decode (s: string): string =
    let rot_90_counter s =
        let rows = s |> extract_rows |> List.map List.rev in
        let rec loop acc xs =
            if List.for_all (fun x -> x = []) xs then List.rev acc
            else let column = List.map List.hd xs in loop (column :: acc) (List.map List.tl xs) in
        let transpose = loop [] rows in
        transpose |> List.map join |> String.concat "\n" in
    let rotated = rot_90_counter s in
    rotated
        |> String.to_seq
        |> List.of_seq
        |> List.filter (fun c -> c <> '\n' && c <> '&')
        |> List.map (String.make 1)
        |> String.concat ""
