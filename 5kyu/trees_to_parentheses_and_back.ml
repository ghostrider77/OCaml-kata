type tree =
    | Leaf
    | Node of tree * tree


let rec tree_to_parens = function
    | Leaf -> ""
    | Node (left, right) -> Printf.sprintf "(%s)%s" (tree_to_parens left) (tree_to_parens right)


let parens_to_tree s =
    let parens = s |> String.to_seq |> List.of_seq in
    let rec convert_parens_to_tree xs =
        let rec loop left_child depth = function
            | [] -> Leaf
            | p :: ps ->
                if p = '(' then loop (p :: left_child) (depth + 1) ps
                else if depth = 1 then
                    let left = convert_parens_to_tree (List.(left_child |> rev |> tl)) in
                    let right = convert_parens_to_tree ps in
                    Node (left, right)
                else loop (p :: left_child) (depth - 1) ps
        in loop [] 0 xs
    in convert_parens_to_tree parens
