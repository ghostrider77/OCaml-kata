module IntMap = Map.Make(
    struct
        type t = int
        let compare = Stdlib.compare
    end
)


let cut_the_ropes (ls : int list) : int list =
    let count_items lst =
        let rec loop acc xs = match xs with
            | [] -> IntMap.bindings acc
            | x :: xss -> match IntMap.find_opt x acc with
                | None -> loop (IntMap.add x 1 acc) xss
                | Some count -> loop (IntMap.add x (count + 1) acc) xss
        in loop IntMap.empty lst in
    let sorted_ropes = List.sort Stdlib.compare ls in
    let mapping = count_items sorted_ropes in
    let occurrences = snd @@ List.split mapping in
    let rec loop acc counts = match counts with
        | [] -> List.rev acc
        | c :: css ->
            let current = List.hd acc in
            let next = current - c in
            if next <= 0 then List.rev acc
            else loop (next :: acc) css
    in loop [List.length ls] occurrences
