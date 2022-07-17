let chain (init_val: 'a) (functions: ('a -> 'a) list): 'a =
  List.fold_left (|>) init_val functions
