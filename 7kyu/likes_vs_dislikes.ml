type like = Like | Dislike


let like_or_dislike (inputs: like list): like option =
    let next_opinion current next = match (current, next) with
        | (Some Like, Like) -> None
        | (Some Dislike, Dislike) -> None
        | _ -> Some next in
    List.fold_left next_opinion None inputs
