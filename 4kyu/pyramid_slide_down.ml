let longest_slide_down (pyramid : int list list) : int =
  let rec pairs = function
    | a :: b :: rest -> (a, b) :: pairs (b :: rest)
    | _ -> [] in
  let n = List.length pyramid in
  let initial = List.init (n + 1) (fun _ -> 0) in
  let process row acc =
    let ps = pairs acc in
    List.map2 (fun c (a, b) -> c + max a b) row ps in
  List.(initial |> fold_right process pyramid |> hd)
