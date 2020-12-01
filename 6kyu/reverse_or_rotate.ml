open Batteries


let revrot str size =
    let sum f s =
        let digits =
            s |> String.to_list
              |> List.map (fun c -> int_of_string @@ Char.escaped c) in
        List.fold_left (fun acc d -> acc + f d) 0 digits in
    let rotate s =
        (String.tail s 1) ^ (String.head s 1) in
    let transform chunk =
        let s = sum (fun x -> x * x * x) chunk in
        let func = if s mod 2 = 0 then String.rev else rotate in
        func chunk in
    let rec loop acc s =
        if String.length s < size then String.concat "" @@ List.rev acc
        else
            let chunk = String.left s size in
            let rest = String.tail s size in
            loop ((transform chunk) :: acc) rest in
    if size <= 0 then "" else loop [] str
