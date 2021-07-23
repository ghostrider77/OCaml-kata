open Batteries

exception MalformedResult of string


type time = { hour : int; minute : int; second : int }


let second_of_time { hour; minute; second } =
    (hour * 60 + minute) * 60 + second


let time_of_second second =
    let minutes = second / 60 in
    let s = second mod 60 in
    let h = minutes / 60 in
    let m = minutes mod 60
    in {hour = h; minute = m; second = s}


let string_of_time {hour; minute; second} =
    let pad t =
        let s = string_of_int t in
        if t < 10 then "0" ^ s else s in
    pad hour ^ "|" ^ pad minute ^ "|" ^ pad second


let calc_statistics seconds =
    let calc_range seconds =
        let (a, b) = List.fold_left (fun (min_s, max_s) s -> (min min_s s, max max_s s)) (max_int, min_int) seconds in
        b - a in
    let calc_mean seconds =
        let n = List.length seconds in
        List.fold_left (+) 0 seconds / n in
    let calc_median seconds =
        let sorted = List.sort compare seconds in
        let n = List.length seconds in
        if n mod 2 = 1 then List.nth sorted (n / 2)
        else ((List.nth sorted (n / 2)) + (List.nth sorted (n / 2 - 1))) / 2
    in (calc_range seconds, calc_mean seconds, calc_median seconds)


let create_summary_from_statistics range mean median =
    let range_result = string_of_time (time_of_second range) in
    let mean_result = string_of_time (time_of_second mean) in
    let median_result = string_of_time (time_of_second median) in
    "Range: " ^ range_result ^ " Average: " ^ mean_result ^ " Median: " ^ median_result


let stat s =
    if String.is_empty s then ""
    else
        let results = String.split_on_string ", " s in
        List.iter (fun x -> print_string x; print_newline()) results;
        let seconds_of_result result =
            let time = result |> String.split_on_char '|' |> List.map int_of_string in
            match time with
                | [h; m; s] -> second_of_time {hour = h; minute = m; second = s}
                | _ -> raise (MalformedResult "Result format is not recognized.") in
        let seconds = List.map seconds_of_result results in
        let (range, mean, median) = calc_statistics seconds
        in create_summary_from_statistics range mean median
