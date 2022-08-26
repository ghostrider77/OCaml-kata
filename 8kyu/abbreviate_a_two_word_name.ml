let abbrev_name (name: string): string =
    let parts = String.split_on_char ' ' name in
    match List.map (fun s -> s.[0]) parts with
        | [f; l] -> Printf.sprintf "%c.%c" (Char.uppercase_ascii f) (Char.uppercase_ascii l)
        | _ -> failwith "Unexpected number of name parts."
