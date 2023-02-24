let to_power_set (set : 'a list) : 'a list list =
    let rec power_set = function
        | [] -> [[]]
        | x :: xs ->
            let ps = power_set xs in
            ps @ (List.map (fun s -> x :: s) ps) in
    power_set set
