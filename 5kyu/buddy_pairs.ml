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


let sum_of_proper_divisors m =
    let canonical_form n =
        let rec loop k p factorization =
            if k = 1 || p > k then factorization
            else
                let p' = if p = 2 then 3 else p + 2 in
                let remainder, exponent = find_largest_exponent k p in
                if exponent = 0 then loop remainder p' factorization
                else loop remainder p' (IntMap.add p exponent factorization) in
        loop n 2 IntMap.empty |> IntMap.bindings in
    let pow n k =
        let rec loop acc alpha =
            if alpha = k then acc
            else loop (acc * n) (alpha + 1) in
        loop 1 0 in
    let factorization = canonical_form m in
    let sum = List.fold_left (fun acc (p, alpha) -> acc * ((pow p (alpha + 1)) - 1) / (p - 1)) 1 factorization in
    sum - m


let buddy_pairs start limit =
    let rec loop n =
        if n > limit then "Nothing"
        else
            let m = (sum_of_proper_divisors n) - 1 in
            if m <= n then loop (n + 1)
            else
                let n' = (sum_of_proper_divisors m) - 1 in
                if n = n' then (string_of_int n) ^ " " ^ (string_of_int m)
                else loop (n + 1)
    in loop start
