module IntSet = Set.Make(
    struct
        type t = int
        let compare = Stdlib.compare
    end
)


let dbl_linear n =
    let generate_next_numbers lst = List.(lst |> map (fun x -> [2*x + 1; 3*x + 1]) |> concat) in
    let get_sequence_elem k =
        let rec loop lst lst_max last_ix =
            let set1 = lst |> generate_next_numbers |> List.filter (fun x -> x > lst_max) |> IntSet.of_list in
            let largest_elem = IntSet.max_elt set1 in
            let small_elems = set1 |> IntSet.filter (fun x -> 2*x + 1 <= largest_elem) |> IntSet.elements in
            let set2 = small_elems |> generate_next_numbers |> IntSet.of_list in
            let set = IntSet.filter (fun x -> x <= largest_elem) (IntSet.union set1 set2) in
            let size = IntSet.cardinal set in
            let next_lst = IntSet.elements set in
            if last_ix + size >= n then List.nth next_lst (n - last_ix - 1)
            else loop next_lst largest_elem (last_ix + size) in
        loop [1] 1 0 in
    if n = 0 then 1
    else get_sequence_elem n
