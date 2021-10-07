open Num

let char_to_int = [('0', 0); ('1', 1); ('2', 2); ('3', 3); ('4', 4); ('5', 5);
               ('6', 6); ('7', 7); ('8', 8); ('9', 9); ('A', 10); ('B', 11);
               ('C', 12); ('D', 13); ('E', 14); ('F', 15); ('G', 16); ('H', 17);
               ('I', 18); ('J', 19); ('K', 20); ('L', 21); ('M', 22); ('N', 23);
               ('O', 24); ('P', 25); ('Q', 26); ('R', 27); ('S', 28); ('T', 29);
               ('U', 30); ('V', 31); ('W', 32); ('X', 33); ('Y', 34); ('Z', 35)]


let int_to_char = List.map (fun (c, v) -> (v, c)) char_to_int


let dec_2_fact_string (nbs: string): string  =
    let rec loop acc k n =
        if n = Int 0 then List.map (String.make 1) acc |> String.concat ""
        else
            let d = quo_num n k in
            let r = int_of_num (mod_num n k) in
            loop ((List.assoc r int_to_char) :: acc) (k +/ Int 1) d
    in loop [] (Int 1) (num_of_string nbs)


let fact_string_2_dec (strg: string): string =
    let digits = String.to_seq strg |> List.of_seq |> List.rev in
    let rec loop acc fact n = function
        | [] -> acc
        | x :: xs ->
            let value = List.assoc x char_to_int in
            loop ((fact */ Int value) +/ acc) (n */ fact) (n +/ Int 1) xs
    in loop (Int 0) (Int 1) (Int 1) digits |> string_of_num
