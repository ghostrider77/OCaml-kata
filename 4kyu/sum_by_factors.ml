module Int =
    struct
        type t = int
        let compare = Stdlib.compare
    end

module IntSet = Set.Make(Int)

module IntMap = Map.Make(Int)


let find_largest_exponent n p =
    let rec loop k exponent =
        if k mod p <> 0 then (k, exponent)
        else loop (k / p) (exponent + 1)
    in loop n 0


let prime_factors n =
    let rec loop k p factors =
        if k = 1 || p > k then factors
        else
            let p' = if p = 2 then p + 1 else p + 2 in
            let remainder, exponent = find_largest_exponent k p in
            if exponent = 0 then loop remainder p' factors
            else loop remainder p' (IntSet.add p factors)
    in loop n 2 IntSet.empty


let sum_of_divided (xs: int list): string =
    let factors = List.map (fun n -> prime_factors (abs n)) xs in
    let all_factors = List.fold_left IntSet.union IntSet.empty factors in
    let add_prime_to_mapping acc p =
        let sum = List.fold_left2 (fun s n ps -> if IntSet.mem p ps then s + n else s) 0 xs factors in
        IntMap.add p sum acc in
    let primes_to_numbers = IntSet.fold (fun p acc -> add_prime_to_mapping acc p) all_factors IntMap.empty in
    let result = IntMap.bindings primes_to_numbers in
    result
        |> List.map (fun (p, s) -> "(" ^ string_of_int p ^ " " ^ string_of_int s ^ ")")
        |> String.concat ""
