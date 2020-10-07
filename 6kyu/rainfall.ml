let convert_to_assoc_list strng =
    let process_line line = match String.split_on_char ':' line with
        | [city; data] ->
            let monthly_data = String.split_on_char ',' data in
            let temperatures =
                List.map (fun s -> float_of_string @@ String.sub s 4 (String.length s - 4)) monthly_data in
                (city, temperatures)
        | _ -> failwith "Invalid string structure" in
    let process_string str =
        let lines = String.split_on_char '\n' str in
        List.map process_line lines in
    process_string strng


let mean town str =
    let mapping = convert_to_assoc_list str in
    match List.assoc_opt town mapping with
        | None -> (-1.0)
        | Some temperatures ->
            (List.fold_left (+.) 0.0 temperatures) /. float_of_int (List.length temperatures)


let variance town str =
    let mapping = convert_to_assoc_list str in
    match List.assoc_opt town mapping with
        | None -> (-1.0)
        | Some temperatures ->
            let n = List.length temperatures in
            let m = (List.fold_left (+.) 0.0 temperatures) /. float_of_int n in
            let s = List.fold_left (fun acc x -> acc +. (x -. m) ** 2.0) 0.0 temperatures in
            s /. (float_of_int n)
