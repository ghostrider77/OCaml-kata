let zeros (n : int) : int =
    let legendre m p =
        let rec loop acc fraction =
            if fraction < 1.0 then acc
            else loop (acc + int_of_float fraction) (fraction /. float p) in
        loop 0 (float m /. float p) in
    let a = legendre n 2 in
    let b = legendre n 5 in
    min a b
