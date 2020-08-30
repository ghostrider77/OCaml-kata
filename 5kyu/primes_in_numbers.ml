module IntMap = Map.Make(
    struct
        type t = int
        let compare = Stdlib.compare
    end
)

let find_largest_exponent n p =
    let rec loop k exponent =
        if k mod p <> 0 then (k, exponent)
        else loop (k / p) (exponent + 1)
    in loop n 0


let canonical_form n =
    let rec loop k p factorization =
        if k = 1 || p > k then factorization
        else
            let p' = if p = 2 then 3 else p + 2 in
            let remainder, exponent = find_largest_exponent k p in
            if exponent = 0 then loop remainder p' factorization
            else loop remainder p' (IntMap.add p exponent factorization)
    in loop n 2 IntMap.empty |> IntMap.bindings


let prime_factors (n: int): string =
    let factorization = canonical_form n in
    let convert_to_string p alpha =
        if alpha = 1 then "(" ^ string_of_int p ^ ")"
        else "(" ^ string_of_int p ^ "**" ^ string_of_int alpha ^ ")" in
    factorization
        |> List.map (fun (p, k) -> convert_to_string p k)
        |> String.concat ""
