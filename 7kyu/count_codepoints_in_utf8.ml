let count_codepoints (utf8 : string) : int =
  let n = String.length utf8 in
  let rec aux acc ix =
    if ix >= n then acc
    else
      let utf8_char = String.get_utf_8_uchar utf8 ix in
      let k = Uchar.utf_decode_length utf8_char in
      aux (acc + 1) (ix + k)
  in aux 0 0
