let comp a b: bool =
  if List.length a <> List.length b then false
  else
      let a_squared = List.map (fun k -> k*k) a in
      List.for_all2 (=) (List.sort compare a_squared) (List.sort compare b)
