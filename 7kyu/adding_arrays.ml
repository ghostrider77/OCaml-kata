let list_adder (l: string list list) =
    let rec loop words lst =
        if List.hd lst = [] then words |> List.rev |> String.concat " "
        else
            let word = lst |> List.map List.hd |> String.concat "" in
            loop (word :: words) (List.map List.tl lst) in
    match l with
        | [] -> ""
        | _ -> loop [] l
