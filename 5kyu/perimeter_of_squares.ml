open Num


let perimeters n =
    let fibonacci m =
        let rec loop a b k =
            if k = m then a
            else loop b (a +/ b) (k + 1) in
        loop (Int 1) (Int 1) 0 in
    let f = fibonacci (n + 2) in
    string_of_num @@ (Int 4) */ (f -/ (Int 1))
