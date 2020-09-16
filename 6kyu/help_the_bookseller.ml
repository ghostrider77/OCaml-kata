module StringMap = Map.Make(
    struct
        type t = string
        let compare = Stdlib.compare
    end
)


exception Invalid_inventory_format


let stock_list (lart: string array) (lcat: string array) =
    let update acc s = match String.split_on_char ' ' s with
        | [code; count] ->
            let chr = Char.escaped code.[0] in
            let acc' = match StringMap.find_opt chr acc with
                | None -> StringMap.add chr (int_of_string count) acc
                | Some c -> StringMap.add chr (c + int_of_string count) acc in
            acc'
        | _ -> raise Invalid_inventory_format in
    let counts = Array.fold_left update StringMap.empty lart in
    if Array.length lart = 0 || Array.length lcat = 0 then ""
    else
        let result =
            Array.map (fun c ->
                let cnt = if StringMap.mem c counts then StringMap.find c counts else 0 in (c, cnt)) lcat in
        result
            |> Array.to_list
            |> List.map (fun (c, cnt) -> "(" ^ c ^ " : " ^ (string_of_int cnt) ^ ")")
            |> String.concat " - "
