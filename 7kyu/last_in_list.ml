let rec last xs = match xs with
    | [] -> None
    | [x] -> Some x
    | _ :: xss -> last xss
