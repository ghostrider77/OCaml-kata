let count_vowels (s: string): int =
    let vowels = ['a'; 'e'; 'i'; 'o'; 'u'] in
    Seq.fold_left (fun acc c -> if List.mem c vowels then acc + 1 else acc) 0 (String.to_seq s)
