let up_to_19 = List.mapi (fun ix s -> (ix + 1, s)) [
    "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"; "ten";
    "eleven"; "twelve"; "thirteen"; "fourteen"; "fifteen"; "sixteen"; "seventeen"; "eighteen"; "nineteen"
    ]
let multiples_of_ten =
    List.mapi (fun ix s-> (ix, s)) ["twenty"; "thirty"; "forty"; "fifty"; "sixty"; "seventy"; "eighty"; "ninety"]


let rec words_of_int n =
    let two_digit_numbers k =
        if k < 20 then List.assoc k up_to_19
        else
            let m = k / 10 in
            let r = k mod 10 in
            let tens = List.assoc (m - 2) multiples_of_ten in
            if r = 0 then tens
            else
                let ones = List.assoc r up_to_19 in
                tens ^ " " ^ ones in
    let three_digit_numbers k =
        let m = k / 100 in
        let r = k mod 100 in
        let hundreds = List.assoc m up_to_19 ^ " hundred" in
        if r = 0 then hundreds
        else hundreds ^ " and " ^ (two_digit_numbers r) in
    if n = 0 then "zero"
    else if n < 100 then two_digit_numbers n
    else if n < 1000 then three_digit_numbers n
    else if n = 1000 then "one thousand"
    else failwith "The input is larger than expected."


let sort (lst: int list): int list =
    List.sort (fun a b -> compare (words_of_int a) (words_of_int b)) lst
