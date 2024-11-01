let trilingual_democracy (group : string) : char =
  let languages = List.of_seq (String.to_seq group) in
  match List.sort compare languages with
    | [a; b; c] when a = b && b = c  -> a
    | ['D'; 'D'; chr] -> chr
    | [chr; 'K'; 'K'] -> chr
    | ['F'; 'F'; chr] | [chr; 'F'; 'F'] | ['I'; 'I'; chr] | [chr; 'I'; 'I'] -> chr
    | ['D'; 'F'; 'I'] -> 'K'
    | ['D'; 'F'; 'K'] -> 'I'
    | ['D'; 'I'; 'K'] -> 'F'
    | ['F'; 'I'; 'K'] -> 'D'
    | _ -> failwith "All cases should be covered."
