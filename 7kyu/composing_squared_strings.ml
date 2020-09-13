let compose(s1: string) (s2: string): string =
    let l1 = String.split_on_char '\n' s1 in
    let l2 = List.rev @@ String.split_on_char '\n' s2 in
    let prefixes = List.mapi (fun ix s -> String.sub s 0 (ix + 1)) l1 in
    let suffixes = List.mapi (fun ix s -> String.sub s 0 (String.length s - ix)) l2 in
    let merged = List.map2 (^) prefixes suffixes in
    String.concat "\n" merged
