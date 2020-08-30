let expression_matter a b c =
    let expressions = [a + b + c; a + (b * c); (a + b) * c; a * b * c; a * (b + c); a * b + c]
    in List.fold_left (fun acc x -> if x > acc then x else acc) min_int expressions
