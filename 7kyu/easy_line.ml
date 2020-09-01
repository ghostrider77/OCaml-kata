open Num


let easyline n =
    let range_product a b =
        let rec loop acc k =
            if k > b then acc
            else loop (acc */ (num_of_int k)) (k + 1)
        in loop (num_of_int 1) a in
    let numer = range_product (n + 1) (2*n) in
    let denom = range_product 1 n in
    string_of_num (numer // denom)
