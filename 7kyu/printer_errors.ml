let char_range first last =
    let rec loop acc c =
        let acc' = c :: acc in
        if c = last then List.rev acc'
        else loop acc' (Char.chr (Char.code c + 1))
    in loop [] first


let printer_error(s: string): string =
    let length = String.length s in
    let seq = String.to_seq s in
    let range = char_range 'a' 'm' in
    let errors = Seq.fold_left (fun acc char -> if List.mem char range then acc else acc + 1) 0 seq
    in (string_of_int errors) ^ "/" ^ (string_of_int length)
