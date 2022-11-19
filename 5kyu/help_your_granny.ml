let tour (friends: string array) (friend_towns: string array array) (h: (string * float) list): int =
    let friend_towns = Array.to_list friend_towns in
    let find_friends_town friend =
        List.find_opt (fun arr -> arr.(0) = friend) friend_towns in
    let rec extract_towns acc = function
        | [] -> List.rev acc
        | f :: fs ->
            match find_friends_town f with
                | None -> extract_towns acc fs
                | Some arr -> extract_towns (arr.(1) :: acc) fs in
    let visited_towns = extract_towns [] (Array.to_list friends) in
    let get_distance town =
        List.assoc town h in
    let rec loop acc a = function
        | [xn] -> int_of_float (acc +. a)
        | x1 :: x2 :: rest ->
            let c = get_distance x2 in
            let b = sqrt (c *. c -. a *. a) in
            loop (acc +. b) c (x2 :: rest)
        | _ -> failwith "empty list of towns" in
    match visited_towns with
        | [] -> 0
        | x :: _ -> let a = get_distance x in loop a a visited_towns
