type subset = {items : int list; sum : int}


let subset_sum (xs : int list) (target : int) : int list option =
  let add_item x {items; sum} =
    let sum' = sum + x in
    if sum' > target then None
    else Some {items = x :: items; sum = sum'} in
  let rec aux acc xs =
    match List.find_opt (fun {sum = s; _} -> s = target) acc with
      | Some {items; _} -> Some (List.rev items)
      | None ->
          match xs with
            | [] -> None
            | x :: rest ->
                let with_x = List.filter_map (add_item x) acc in
                let acc' = List.sort_uniq (fun {sum = s1; _} {sum = s2; _} -> compare s1 s2) (acc @ with_x) in
                aux acc' rest in
  aux [{items = []; sum = 0}] xs
