let longest_consec (xs: string list) (k: int): string =
    let arr = Array.of_list xs in
    let n = Array.length arr in
    let rec loop acc s =
        if s + k > n then acc
        else
            let subarray = Array.sub arr s k in
            let substring = subarray |> Array.to_list |> String.concat "" in
            let acc' = if String.length substring > String.length acc then substring else acc in
            loop acc' (s + 1) in
    if k <= 0 then ""
    else loop "" 0
