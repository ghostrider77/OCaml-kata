type bearing = Bearing of int


let identify_bearing (bearings : bearing list) (weigh : bearing list -> int) : bearing =
  let n = List.length bearings in
  let zipped = List.mapi (fun ix b -> (ix + 1, b)) bearings in
  let selected = List.concat_map (fun (ix, b) -> List.init ix (fun _ -> b)) zipped in
  let s = 5 * n * (n + 1) in
  Bearing (weigh selected - s - 1)
