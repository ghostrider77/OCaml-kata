let determinant (matrix: int array array): int =
    let rec loop = function
        | [] -> failwith "Empty matrix."
        | [[a]] -> a
        | first_row :: rest ->
            let process det (a, jy) =
                if a = 0 then det
                else
                    let remove_item j xs = List.(
                        xs |> mapi (fun i x -> (i, x)) |> filter (fun (i, x) -> i <> j) |> map (fun (_, x) -> x)) in
                    let submatrix = List.map (remove_item jy) rest in
                    let sign = if jy mod 2 = 0 then 1 else -1 in
                    det + sign * a * loop submatrix in
            List.fold_left process 0 (List.mapi (fun jy a -> (a, jy)) first_row)
    in loop Array.(matrix |> map to_list |> to_list)
