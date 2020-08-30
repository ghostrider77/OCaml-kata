let range a b s =
    let rec loop acc k =
        if k > b then List.rev acc
        else loop (k :: acc) (k + s)
    in loop [] a


let is_prime n =
    if n = 2 then true
    else if n = 1 || n mod 2 = 0 then false
    else
        let limit = n |> float_of_int |> sqrt |> floor |> int_of_float
        in List.for_all (fun k -> n mod k <> 0) (range 3 limit 2)


let get_first_prime m =
    let rec loop k =
        if is_prime k then k
        else loop (k + 1)
    in loop m


let gap (g: int) (m: int) (n: int): (int * int) option =
    let p0 = get_first_prime m in
    let rec loop p k =
        if k > n then None
        else if is_prime k then
            if k - p = g then Some (p, k)
            else loop k (k + 2)
        else loop p (k + 1)
    in loop p0 (p0 + 2)
