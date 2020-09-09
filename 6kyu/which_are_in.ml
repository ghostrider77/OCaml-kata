open Batteries


let inArray(a1: string list) (a2: string list) =
    let rec loop acc = function
        | [] -> List.sort_uniq compare acc
        | x :: xss -> match List.find_opt (fun s -> String.exists s x) a2 with
            | None -> loop acc xss
            | Some _ -> loop (x :: acc) xss
    in loop [] a1
