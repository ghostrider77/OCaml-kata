exception Exception of string


let begin_ cmd = cmd []


let push stack x cmd  = cmd (x :: stack)


let add stack cmd = match stack with
    | x :: y :: rest -> cmd ((x + y) :: rest)
    | _ -> raise (Exception "Not enough elements in stack.")


let end_ = function
    | [] -> raise (Exception "Empty stack")
    | x :: _ -> x
