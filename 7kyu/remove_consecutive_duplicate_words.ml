let remove_consecutive_duplicates (str : string) : string =
    let rec loop acc current = function
        | [] -> List.rev (current :: acc)
        | word :: words ->
            if current = word then loop acc current words
            else loop (current :: acc) word words in
    match String.split_on_char ' ' str with
        | [] -> str
        | w :: ws -> String.concat " " (loop [] w ws)
