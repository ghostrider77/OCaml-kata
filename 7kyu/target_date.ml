module Date : sig
    type t
    val create : int -> int -> int -> t
    val next_day : t -> t
    val to_string : t -> string
end = struct
    type t = {year : int; month : int; day : int}

    let create year month day = {year; month; day}

    let is_leap_year year =
        if year mod 4 <> 0 then false
        else if year mod 100 <> 0 then true
        else year mod 400 = 0

    let next_day {year; month; day} =
        let day' = match (month, day) with
            | (2, 28) -> if is_leap_year year then 29 else 1
            | (2, 29) -> 1
            | (_, 31) -> 1
            | (_, 30) when List.mem month [4; 6; 9; 11] -> 1
            | (_, _) -> day + 1 in
        let month' = if day' = 1 then (month mod 12) + 1 else month in
        let year' = if month' = 1 && day' = 1 then year + 1 else year in
        {year = year'; month = month'; day = day'}

    let to_string {year; month; day} =
        let y = string_of_int year in
        let m = if month < 10 then "0" ^ string_of_int month else string_of_int month in
        let d = if day < 10 then "0" ^ string_of_int day else string_of_int day in
        y ^ "-" ^ m ^ "-" ^ d
end


let date_nb_days (a0: float) (a: float) (p: float): string =
    let rec loop date amount =
        if amount >= a then date
        else loop (Date.next_day date) (amount *. (1.0 +. p /. 36000.0)) in
    let target_date = loop (Date.create 2016 1 1) a0 in
    Date.to_string target_date
