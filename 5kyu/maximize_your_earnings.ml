let maximise_earnings (earnings: int list) (k: int): int =
  let cache = Hashtbl.create 10000 in
  let rec func l = function
    | [] -> 0
    | (x :: rest) as xs ->
        if l = 0 then func k rest
        else match (Hashtbl.find_opt cache (xs, l)) with
          | Some v -> v
          | None ->
              let result = max (func k rest) (x + func (l - 1) rest) in
              Hashtbl.add cache (xs, l) result;
              result in
  func k earnings
