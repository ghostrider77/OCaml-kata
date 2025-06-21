let seqlist (first: int) (step: int) (len: int): int list =
  List.init len (fun k -> first + k * step)
