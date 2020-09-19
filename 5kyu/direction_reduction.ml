let reduce_dir (ls: string list): string list =
    let opposites d1 d2 =
        (d1 = "WEST" && d2 = "EAST") ||
        (d1 = "EAST" && d2 = "WEST") ||
        (d1 = "NORTH" && d2 = "SOUTH") ||
        (d1 = "SOUTH" && d2 = "NORTH") in
    let rec loop stack = function
        | [] -> List.rev stack
        | x :: xss -> match stack with
            | [] -> loop [x] xss
            | y :: yss ->
                if opposites x y then loop yss xss
                else loop (x :: stack) xss
    in loop [] ls
