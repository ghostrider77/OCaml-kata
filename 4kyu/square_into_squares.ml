let decompose (n : int) : int list =
    let rec partition (number : int) (can_be_itself : bool) : (int list) option =
        if number = 0 then Some []
        else if number = 1 then Some [1]
        else
            let upper_limit = (if can_be_itself then number else number - 1) |> float |> sqrt |> int_of_float in
            let rec loop k =
                if k = 0 then None
                else
                    let number' = number - k * k in
                    match partition number' true with
                        | None -> loop (k - 1)
                        | Some squares ->
                            if List.mem k squares then loop (k - 1)
                            else Some (k :: squares) in
            loop upper_limit in
    match partition (n * n) false with
        | None -> []
        | Some result -> List.sort compare result
