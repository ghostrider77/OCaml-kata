let is_subsequence (haystack : string) (needle : string) : bool =
  let haystack = String.lowercase_ascii haystack in
  let n = String.length haystack in
  let rec loop ix = function
    | [] -> true
    | c :: cs ->
        if ix >= n then false
        else
          match String.index_from_opt haystack ix c with
            | None -> false
            | Some ix' -> loop (ix' + 1) cs in
  loop 0 (needle |> String.lowercase_ascii |> String.to_seq |> List.of_seq)
