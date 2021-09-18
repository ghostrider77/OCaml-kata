let arrange (s: string) =
    let words = String.split_on_char ' ' s in
    let rec loop acc ix h = function
        | [] -> List.rev (h :: acc)
        | w :: ws ->
            let lh = String.length h in
            let lw = String.length w in
            if (ix mod 2 = 0 && lh > lw) || (ix mod 2 = 1 && lh < lw)
            then loop (w :: acc) (ix + 1) h ws
            else loop (h :: acc) (ix + 1) w ws in
    let reordered = loop [] 0 (List.hd words) (List.tl words) in
    let change_case ix w = if ix mod 2 = 0 then String.lowercase_ascii w else String.uppercase_ascii w in
    List.mapi change_case reordered |> String.concat " "
