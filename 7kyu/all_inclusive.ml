let contain_all_rots strng arr =
    let n = String.length strng in
    let double_str = strng ^ strng in
    let rec loop acc k =
        if k = n then acc
        else
            let rotated = String.sub double_str k n in
            loop (rotated :: acc) (k + 1) in
    let rotations = loop [] 0 in
    List.for_all (fun s -> List.mem s arr) rotations
