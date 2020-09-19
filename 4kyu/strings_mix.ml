module CharMap = Map.Make(Char)

module CharSet = Set.Make(Char)

type maximum_occurrence = { letter : char; count : int; arg : char }


let count_lowercase s =
    let is_lowercase_char c =
        let code = Char.code c in
        97 <= code && code <= 122 in
    let lowercase_chars = s |> String.to_seq |> Seq.filter is_lowercase_char |> List.of_seq in
    let update counts c = match CharMap.find_opt c counts with
        | None -> CharMap.add c 1 counts
        | Some v -> CharMap.add c (v + 1) counts in
    List.fold_left update CharMap.empty lowercase_chars


let sort_maximum_occurrences maximums =
    let compare m1 m2 =
        if m1.count < m2.count then 1
        else if m1.count > m2.count then -1
        else if m1.arg < m2.arg then -1
        else if m1.arg > m2.arg then 1
        else Stdlib.compare m1.letter m2.letter in
    List.sort compare maximums


let stringify maximums =
    let represent {letter; count; arg} =
        (Char.escaped arg) ^ ":" ^ String.make count letter in
    maximums |> List.map represent |> String.concat "/"


let mix (s1: string) (s2: string): string =
    let get_counts s =
        s |> count_lowercase |> CharMap.filter (fun _ c -> c > 1) in
    let get_keys m1 m2 =
        let keys1 = CharMap.fold (fun k _ acc -> k :: acc) m1 [] in
        let keys2 = CharMap.fold (fun k _ acc -> k :: acc) m2 [] in
        CharSet.union (CharSet.of_list keys1) (CharSet.of_list keys2) in
    let counts1 = get_counts s1 in
    let counts2 = get_counts s2 in
    let common_chars = get_keys counts1 counts2 in
    let get_or_else m k = match CharMap.find_opt k m with
        | None -> 0
        | Some v -> v in
    let select_maximum c acc =
        let c1 = get_or_else counts1 c in
        let c2 = get_or_else counts2 c in
        let char_maximum_occurrence =
            if c1 > c2 then { letter = c; count = c1; arg = '1' }
            else if c1 < c2 then { letter = c; count = c2; arg = '2' }
            else { letter = c; count = c1; arg = '=' } in
        char_maximum_occurrence :: acc in
    let maximums = CharSet.fold select_maximum common_chars [] in
    let sorted_maximums = sort_maximum_occurrences maximums in
    stringify sorted_maximums
