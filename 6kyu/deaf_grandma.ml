let deaf_grandma (you: string list): string list =
  let rec loop acc = function
    | [] -> List.rev acc
    | words :: rest ->
        if words = "BYE" then List.rev ("OK, BYE!" :: acc)
        else if String.for_all (fun c -> c = Char.uppercase_ascii c) words then loop ("NO, NOT SINCE 1938!" :: acc) rest
        else loop ("HUH?! SPEAK UP, SONNY!" :: acc) rest in
  loop [] you