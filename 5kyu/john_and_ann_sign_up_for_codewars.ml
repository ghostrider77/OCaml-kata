let john n =
    let a = Array.make n 0 in
    let j = Array.make n 0 in
    a.(0) <- 1;
    let rec loop k =
        if k >= n then j
        else (
            a.(k) <- k - j.(a.(k - 1));
            j.(k) <- k - a.(j.(k - 1));
            loop (k + 1))
    in loop 1


let ann n =
    let a = Array.make n 0 in
    let j = Array.make n 0 in
    a.(0) <- 1;
    let rec loop k =
        if k >= n then a
        else (
            a.(k) <- k - j.(a.(k - 1));
            j.(k) <- k - a.(j.(k - 1));
            loop (k + 1))
    in loop 1


let sum_john n =
    let array = john n in
    Array.fold_left (+) 0 array


let sum_ann n =
    let array = ann n in
    Array.fold_left (+) 0 array
