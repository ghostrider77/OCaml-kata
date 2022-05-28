type status = Buy | Sell

type order = { quote : string; quantity : int; price : float; status : status }


let status_of_char = function
    | 'B' -> Buy
    | 'S' -> Sell
    | chr -> raise @@ Invalid_argument ("Unknown status: " ^ (String.make 1 chr))



let balance_statements (str : string) : string =
    let parse_order line =
        try
            let (quote, quantity, price, c) = Scanf.sscanf line "%s %d %s %c" (fun a b c d -> (a, b, c, d)) in
            if String.contains price '.'
            then Some {quote; quantity; price = float_of_string price; status = status_of_char c}
            else None
        with _ -> None in
    let rec loop ((b, s) as acc) badly_formed = function
        | [] -> ((b, s), List.rev badly_formed)
        | x :: xs ->
            match parse_order x with
                | None -> loop acc (x :: badly_formed) xs
                | Some {quantity; price; status} ->
                    let acc' =
                        if status = Buy then (b +. float quantity *. price, s)
                        else (b, s +. float quantity *. price) in
                    loop acc' badly_formed xs in
    let round x = int_of_float (x +. 0.5) in
    let items = str |> String.split_on_char ',' |> List.map String.trim in
    let ((buy, sell), ill_formed) = if str = "" then ((0.0, 0.0), []) else loop (0.0, 0.0) [] items in
    let first_part = "Buy: " ^ string_of_int (round buy) ^ " Sell: " ^ string_of_int (round sell) in
    match ill_formed with
        | [] -> first_part
        | _ ->
            let n = List.length ill_formed in
            let second_part = "; Badly formed " ^ string_of_int n ^ ": " in
            let third_part = String.concat " ;" ill_formed in
            first_part ^ second_part ^ third_part ^ " ;"
