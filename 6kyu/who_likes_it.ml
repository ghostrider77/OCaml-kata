let likes (names : string list) : string =
  match names with
    | [] -> "no one likes this"
    | [name] -> Printf.sprintf "%s likes this" name
    | [name1; name2] -> Printf.sprintf "%s and %s like this" name1 name2
    | [name1; name2; name3] -> Printf.sprintf "%s, %s and %s like this" name1 name2 name3
    | name1 :: name2 :: others -> Printf.sprintf "%s, %s and %d others like this" name1 name2 (List.length others)
