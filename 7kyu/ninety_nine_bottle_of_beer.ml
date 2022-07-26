let sing (): string list =
    let first_line k =
        if k = 1 then "1 bottle of beer on the wall, 1 bottle of beer."
        else Printf.sprintf "%d bottles of beer on the wall, %d bottles of beer." k k in
    let second_line k =
        if k = 1 then "Take one down and pass it around, no more bottles of beer on the wall."
        else if k = 2 then "Take one down and pass it around, 1 bottle of beer on the wall."
        else Printf.sprintf "Take one down and pass it around, %d bottles of beer on the wall." (k - 1) in
    let rec loop acc k =
        if k = 0 then
            let penultimate_line = "No more bottles of beer on the wall, no more bottles of beer." in
            let last_line = "Go to the store and buy some more, 99 bottles of beer on the wall." in
            List.rev (last_line :: penultimate_line :: acc)
        else loop ((second_line k) :: (first_line k) :: acc) (k - 1)
    in loop [] 99
