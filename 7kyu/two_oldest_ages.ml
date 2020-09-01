exception Short_list


let two_oldest_ages ages = match List.sort (fun x y -> compare y x) ages with
    | a :: b :: _ -> [b; a]
    | _ -> raise Short_list
