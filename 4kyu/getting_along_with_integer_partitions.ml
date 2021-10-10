module IntSet = Set.Make(Int)

module Partitions = Set.Make(
    struct
        type t = int list
        let compare = Stdlib.compare
    end
)


let enum n =
    let rec range a b =
        if a >= b then []
        else a :: (range (a + 1) b) in
    let cache = Array.make n Partitions.empty in
        let extend_partitions k =
            let xs = range 1 k in
            let extend l =
                let ps = cache.(l - 1) in
                let h = k - l in
                Partitions.(ps |> filter (fun items -> h >= List.hd items) |> map (fun items -> h :: items)) in
            List.fold_left (fun acc l -> Partitions.union acc (extend (k - l))) Partitions.empty xs in
    let rec loop k =
        if k = 1 then (
            cache.(k - 1) <- Partitions.singleton [1];
            loop (k + 1)
            )
        else if k > n then Partitions.elements cache.(n - 1)
        else
            let partitions = extend_partitions k in (
                cache.(k - 1) <- (Partitions.add [k] partitions);
                loop (k + 1)
                )
    in loop 1


let part n =
    let round2 n = Float.round (n *. 100.0) /. 100.0 in
    let range xs =
        let (a, b) = List.fold_left (fun (min_s, max_s) s -> (min min_s s, max max_s s)) (max_int, min_int) xs in
        b - a in
    let mean xs =
        let n = List.length xs in
        float (List.fold_left (+) 0 xs) /. (float n) in
    let median xs =
        let sorted = List.sort compare xs in
        let n = List.length xs in
        if n mod 2 = 1 then float @@ List.nth sorted (n / 2)
        else (float ((List.nth sorted (n / 2)) + (List.nth sorted (n / 2 - 1)))) /. 2.0 in
    let partitions = enum n in
    let prod xs = List.fold_left ( * ) 1 xs in
    let products = List.fold_left (fun acc p -> IntSet.add (prod p) acc) IntSet.empty partitions in
    let ps = IntSet.elements products in
    let range_string = string_of_int (range ps) in
    let mean_string = Printf.sprintf "%.2f" (round2 @@ mean ps) in
    let median_string = Printf.sprintf "%.2f" (round2 @@ median ps) in
    "Range: " ^ range_string ^ " Average: " ^ mean_string ^ " Median: " ^ median_string
