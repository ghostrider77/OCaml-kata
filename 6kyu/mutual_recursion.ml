let rec f (n : int) : int =
  if n = 0 then 1
  else n - m (f (n - 1))
and m (n : int) : int =
  if n = 0 then 0
  else n - f (m (n - 1))
