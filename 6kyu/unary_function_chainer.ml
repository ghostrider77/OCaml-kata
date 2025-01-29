let chain (unary_funcs: ('a -> 'a) list) : ('a -> 'a) =
  List.fold_left (fun acc f -> function x -> f (acc x)) Fun.id unary_funcs
