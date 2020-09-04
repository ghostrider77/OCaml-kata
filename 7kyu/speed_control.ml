let gps (s: int) (l: float list): int = match l with
    | ([] | [_]) -> 0
    | h :: tl ->
        let max_delta, _ =
            List.fold_left
                (fun (md, d0) d1 -> let diff = d1 -. d0 in if diff > md then (diff, d1) else (md, d1))
                (min_float, h) tl
        in max_delta /. (float_of_int s) *. 3600.0 |> floor |> int_of_float
