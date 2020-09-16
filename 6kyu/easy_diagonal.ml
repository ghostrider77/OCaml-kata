open Num


let diagonal (n:int) (p:int): string =
    let f n k =
        let rec loop acc i =
            if i > k then string_of_num acc
            else loop (acc */ (n -/ i +/ Int 1) // i ) (i +/ Int 1)
        in loop (Int 1) (Int 1)
    in f (Int (n + 1)) (Int (p + 1))
