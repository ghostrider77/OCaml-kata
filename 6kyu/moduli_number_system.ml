let from_nb_2_str (n: int) (ms: int list): string =
    let rec calc_gcd a b =
        if b = 0 then a else calc_gcd b (a mod b) in
    let is_coprime a b =
        let g = calc_gcd a b in
        g = 1 in
    let pairs lst =
        let rec loop acc = function
            | ([] | [_]) -> acc
            | x :: xss ->
                let x_pairs = List.map (fun y -> (x, y)) xss in
                loop (acc @ x_pairs) xss in
        loop [] lst in
    let valid_system xs =
        let ps = pairs ms in
        let prod = List.fold_left ( * ) 1 ms in
        prod >= n && List.for_all (fun (x, y) -> is_coprime x y) ps in
    if valid_system ms then
        let rs = List.map (fun m -> n mod m) ms in
        let repr = rs |> List.map string_of_int |> String.concat "--" in
        "-" ^ repr ^ "-"
    else "Not applicable"
