let squares_in_rect (lng: int) (wdth: int): int list option =
    let rec loop acc a b =
        if a = b then List.rev (a :: acc)
        else
            let longer = max a b in
            let shorter = min a b in
            loop (shorter :: acc) shorter (longer - shorter) in
    if lng = wdth then None
    else Some (loop [] lng wdth)
