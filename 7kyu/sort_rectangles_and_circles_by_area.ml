type shape =
    | Rectangle of float * float
    | Circle of float


let sort_by_area (shapes: shape list): shape list =
    let area = function
        | Rectangle (a, b) -> a *. b
        | Circle r -> r  *. r *. Float.pi in
    List.sort (fun s1 s2 -> compare (area s1) (area s2)) shapes
