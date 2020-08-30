let powersOfTwo n =
    let rec loop acc k =
        if k = 0 then loop [1] (k + 1)
        else if k > n then List.rev acc
        else loop ((2 * List.hd acc) :: acc) (k + 1)
    in loop [] 0
