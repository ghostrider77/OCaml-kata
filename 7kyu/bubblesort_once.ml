let bubblesort_once (t: int array): int array =
    let rec loop acc = function
        | a :: b :: xss -> if a < b then loop (a :: acc) (b :: xss) else loop (b :: acc) (a :: xss)
        | [a] -> loop (a :: acc) []
        | [] -> acc |> List.rev |> Array.of_list in
    loop [] (Array.to_list t)
