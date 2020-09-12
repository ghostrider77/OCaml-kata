let scale (s: string) (hcount: int) (vcount: int) =
    if s = "" then ""
    else
        let lst = String.split_on_char '\n' s in
        let explode k s =
            s |> String.to_seq |> List.of_seq |> List.map (String.make k) |> String.concat "" in
        let h_scaled = List.map (explode hcount) lst in
        let v_scaled = List.(h_scaled |> map (fun x -> List.init vcount (fun _ -> x)) |> concat) in
        String.concat "\n" v_scaled
