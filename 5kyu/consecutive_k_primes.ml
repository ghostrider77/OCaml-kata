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


let consec_kprimes k arr =
    let is_k_prime n =
        let factorization = canonical_form n in
        let nr_factors = List.fold_left (fun acc (_, alpha) -> acc + alpha) 0 factorization in
        nr_factors = k in
    let item_is_k_prime = List.map is_k_prime arr in
    let rec loop acc = function
        | ([] | [_]) -> acc
        | a :: b :: rest ->
            let acc' = if a && b then acc + 1 else acc in
            loop acc' (b :: rest)
    in loop 0 item_is_k_prime
