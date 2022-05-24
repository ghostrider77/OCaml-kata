open Batteries


let josephus (xs : 'a list) (k : int): 'a list =
    let items = DynArray.of_list xs in
    let rec loop acc ix size =
        if size = 0 then List.rev acc
        else
            let next_ix = (ix - 1 + k) mod size in
            let item = DynArray.get items next_ix in
            DynArray.delete items next_ix;
            loop (item :: acc) next_ix (size - 1)
    in loop [] 0 (List.length xs)
