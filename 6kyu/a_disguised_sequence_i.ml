open Num


let fcn(n:int): string =
    let rec power_of_two acc k =
        if k = n then acc
        else power_of_two (acc */ Int 2) (k + 1)
    in string_of_num (power_of_two (Int 1) 0)
