type char_info = { first_occurrence : int; count : int }

module CharMap = Map.Make(
    struct
        type t = char
        let compare = Stdlib.compare
    end
)

let first_non_repeating_letter str =
    let lowercase_chars = String.(str |> lowercase_ascii |> to_seq) |> List.of_seq in
    let process_char acc (ix, c) =
        let increment_count = function
            | None -> Some { first_occurrence = ix; count = 1 }
            | Some ({ count } as record) -> Some { record with count = count + 1 }
        in CharMap.update c increment_count acc in
    let char_counts =
        List.(lowercase_chars |> mapi (fun ix c -> (ix, c)) |> fold_left process_char CharMap.empty) in
    let non_repeating_chars = CharMap.filter (fun _ { count } -> count = 1) char_counts in
    let seq = CharMap.to_seq non_repeating_chars in
    let char_occurrences = List.of_seq @@ Seq.map (fun (c, { first_occurrence }) -> (c, first_occurrence)) seq in
    match char_occurrences with
        | [] -> None
        | first :: rest ->
            let (_, min_index) =
                List.fold_left (fun ((min_c, min_ix) as acc) (c, ix) -> if ix < min_ix then (c, ix) else acc) first rest
            in Some str.[min_index]
