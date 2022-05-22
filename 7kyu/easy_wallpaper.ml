let wall_paper (l: float) (w: float) (h: float): string =
    let numbers = [
        "zero"; "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine";
        "ten"; "eleven"; "twelve"; "thirteen"; "fourteen"; "fifteen"; "sixteen"; "seventeen"; "eighteeen";
        "nineteen"; "twenty"
    ] in
    let nr_rolls =
        if l = 0.0 || w = 0.0 || h = 0.0 then 0
        else
            let area = (l +. w) *. h *. 2.0 *. 1.15 /. 5.2 in
            area |> ceil |> int_of_float in
    List.nth numbers nr_rolls
