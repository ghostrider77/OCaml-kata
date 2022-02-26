
let get_count (s: string): int =
    let vowels = ['a'; 'e'; 'i'; 'o'; 'u'] in
    s |> String.to_seq |> Seq.fold_left (fun acc c -> if List.mem c vowels then acc + 1 else acc) 0
