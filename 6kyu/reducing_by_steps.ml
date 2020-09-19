let som = (+)

let rec gcdi u v =
    let rec calc_gcd a b =
        if b = 0 then a else calc_gcd b (a mod b) in
    calc_gcd (abs u) (abs v)

let lcmu m n =
    let g = gcdi m n
    in ((abs m) / g) * (abs n)

let maxi = max

let mini = min

let oper fct arr init =
    let rec loop (acc, r) = function
        | [] -> List.rev acc
        | x :: xss ->
            let r' = fct r x in
            loop (r' :: acc, r') xss
    in loop ([], init) arr
