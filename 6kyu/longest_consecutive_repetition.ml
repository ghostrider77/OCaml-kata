let longest_consecutive_repetition (s : string) : (char * int) option =
    let rec loop acc current_char current_count = function
        | [] ->
            let acc' = (current_char, current_count) :: acc in
            let keep_largest (chr_max, cnt_max) (c, cnt) = if cnt >= cnt_max then (c, cnt) else (chr_max, cnt_max) in
            Some (List.fold_left keep_largest (' ', 0) acc')
        | c :: css ->
            if c = current_char then loop acc current_char (current_count + 1) css
            else loop ((current_char, current_count) :: acc) c 1 css in
    match s |> String.to_seq |> List.of_seq with
        | [] -> None
        | chr :: chars -> loop [] chr 1 chars
