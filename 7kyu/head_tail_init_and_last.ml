exception Empty_list of string


let head  = function
    | [] -> raise (Empty_list "Head of empty list")
    | x :: xss -> x


let tail = function
    | [] -> raise (Empty_list "Tail of empty list")
    | x :: xss -> xss


let rec last = function
    | [] -> raise (Empty_list "Last of empty list")
    | [x] -> x
    | x :: xs -> last xs


let init xs =
    let rec loop acc = function
        | [] -> raise (Empty_list "Init of empty list")
        | [x] -> List.rev acc
        | x :: xs -> loop (x :: acc) xs
    in loop [] xs
