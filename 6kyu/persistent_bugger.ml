let persistence (n : int) : int =
  let rec aux acc k =
    let s = string_of_int k in
    if String.length s = 1 then acc
    else
      let digits = s |> String.to_seq |> Seq.map (fun d -> int_of_string (String.make 1 d)) in
      let k' = Seq.fold_left ( * ) 1 digits in
      aux (acc + 1) k' in
  aux 0 n
