type time = { hour : string; minute : string }


let rec permutation xs =
    let rec interleave x = function
        | [] -> [[x]]
        | (y :: ys) ->
            let tl = List.map (fun z -> y :: z) (interleave x ys) in
            (x :: y :: ys) :: tl in
    match xs with
        | [] -> [[]]
        | (x :: xss) -> List.concat_map (interleave x) (permutation xss)


let latest_clock (a: int) (b: int) (c: int) (d: int) : string =
    let time_of_digits = function
        | [d1; d2; d3; d4] ->
            let h = string_of_int d1 ^ string_of_int d2 in
            let m = string_of_int d3 ^ string_of_int d4 in
            if h < "24" && m < "60" then Some {hour = h; minute = m}
            else None
        | _ -> failwith "Malformed input" in
    let possible_times = List.filter_map time_of_digits (permutation [a; b; c; d]) in
    match List.sort (fun t1 t2 -> compare t2 t1) possible_times with
        | {hour; minute} :: _ -> hour ^ ":" ^ minute
        | _ -> failwith "No solution exist."
