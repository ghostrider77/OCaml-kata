let rotate_left k arr =
    let length = Array.length arr in
    if k < length - 1 then
        begin
            let last = arr.(k) in
            for ix = k to length - 2 do
                arr.(ix) <- arr.(ix+1)
            done;
            arr.(length-1) <- last
        end;
    arr

let max_rot (n: int): int =
    let digits = n |> string_of_int |> String.to_seq |> Array.of_seq |> Array.map Char.escaped in
    let length = Array.length digits in
    let evaluate string_array = string_array |> Array.to_list |> String.concat "" |> int_of_string in
    let rec loop ((rotated, max_value) as acc) k =
        if k = length - 1 then acc
        else
            let rotated' = rotate_left k rotated in
            let v = evaluate rotated' in
            let acc' = if v > max_value then (rotated', v) else acc
            in loop acc' (k + 1) in
    let _, max_value = loop (digits, evaluate digits) 0
    in max_value
