
module Duration : sig
    type t
    val of_seconds : int -> t
    val to_string : t -> string
end = struct
    type t = { years : int; days : int; hours : int; minutes : int; seconds : int }

    let of_seconds s =
        let seconds = s mod 60 in
        let rest = s / 60 in
        let minutes = rest mod 60 in
        let rest = rest / 60 in
        let hours = rest mod 24 in
        let rest = rest / 24 in
        let days = rest mod 365 in
        let years = rest / 365 in
        {years; days; hours; minutes; seconds}

    let to_string {years; days; hours; minutes; seconds} =
        let string_of_item count name =
            if count = 0 then ""
            else if count = 1 then (string_of_int 1) ^ " " ^ name
            else (string_of_int count) ^ " " ^ name ^ "s" in
        let text = [
            string_of_item years "year";
            string_of_item days "day";
            string_of_item hours "hour";
            string_of_item minutes "minute";
            string_of_item seconds "second"
            ] in
        match List.(text |> filter (fun x -> x <> "") |> rev) with
            | [] -> "now"
            | [x] -> x
            | [y; x] -> x ^ " and " ^ y
            | y :: x :: zs -> (String.concat ", " (List.rev zs)) ^ ", " ^ x ^ " and " ^ y
end


let format_duration (seconds: int): string =
    let duration = Duration.of_seconds seconds in
    Duration.to_string duration
