let remove lst k =
    let zipped = List.mapi (fun ix x -> (ix, x)) lst in
    let rec loop acc = function
        | [] -> List.rev acc
        | (ix, x) :: rest ->
            if ix = k then loop acc rest
            else loop (x :: acc) rest in
    (loop [] zipped, List.nth lst k)


let insert lst k item =
    let rec loop acc ix = function
        | [] -> List.rev acc
        | x :: xs ->
            if ix = k then loop (x :: item :: acc) (ix + 1) xs
            else loop (x :: acc) (ix + 1) xs in
    if k >= List.length lst then lst @ [item]
    else loop [] 0 lst


let smallest(sn: string): string =
    let length = String.length sn in
    let digit_list = sn |> String.to_seq |> List.of_seq |> List.map (String.make 1) in
    let calc_index_pairs n =
        let lst = List.init n (fun ix -> ix) in
        List.(lst |> map (fun ix -> List.init n (fun jy -> (ix, jy))) |> concat) in
    let index_pairs = calc_index_pairs length in
    let process ((smallest_list, (min_ix, min_jy)) as acc) (ix, jy) =
        let removed, item = remove digit_list ix in
        let inserted = insert removed jy item in
        if inserted < smallest_list then (inserted, (ix, jy))
        else acc in
    let maximal_number = List.init length (fun _ -> "9") in
    let result, (a, b) = List.fold_left process (maximal_number, (0, 0)) index_pairs in
    let result_number = result |> String.concat "" |> int_of_string in
    (string_of_int result_number) ^ " " ^ (string_of_int a) ^ " " ^ (string_of_int b)
