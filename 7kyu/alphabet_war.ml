let alphabet_war (fight : string) : string =
  let powers = [('w', 4); ('p', 3); ('b', 2); ('s', 1); ('m', -4); ('q', -3); ('d', -2); ('z', -1)] in
  let get_left_score chr =
    Option.value (List.assoc_opt chr powers) ~default:0 in
  let result = String.fold_left (fun acc c -> acc + get_left_score c) 0 fight in
  if result > 0 then "Left side wins!"
  else if result = 0 then "Let's fight again!"
  else "Right side wins!"
