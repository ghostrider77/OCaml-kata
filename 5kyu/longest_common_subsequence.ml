let backtrack_lcs backtrack s n1 n2 =
    let rec loop acc i j =
        if i <= 0 || j <= 0 then acc
        else if backtrack.(i-1).(j-1) = 0 then loop (s.(i-1) :: acc) (i - 1) (j - 1)
        else if backtrack.(i-1).(j-1) = -1 then loop acc (i - 1) j
        else loop acc i (j - 1)
    in loop [] n1 n2


let lcs (s1: 'a list) (s2: 'a list) =
    let s1 = Array.of_list s1 in
    let s2 = Array.of_list s2 in
    let n1 = Array.length s1 in
    let n2 = Array.length s2 in
    let longest_path = Array.make_matrix (n1 + 1) (n2 + 1) 0 in
    let backtrack = Array.make_matrix n1 n2 0 in
    for i = 0 to (n1 - 1) do
        for j = 0 to (n2 - 1) do
            let mismatch = if s1.(i) = s2.(j) then 1 else 0 in
            let path = max (max longest_path.(i).(j+1) longest_path.(i+1).(j)) (longest_path.(i).(j) + mismatch) in
            let direction =
                if path = longest_path.(i).(j+1) then -1
                else if path = longest_path.(i+1).(j) then 1
                else 0 in
            longest_path.(i+1).(j+1) <- path;
            backtrack.(i).(j) <- direction
        done;
    done;
    backtrack_lcs backtrack s1 n1 n2
