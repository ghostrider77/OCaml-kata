open Batteries

exception Invalid_address


let travel address_string zipcode =
    if zipcode = "" then ":/"
    else
        let split_on_house_number address = match String.split_on_char ' ' address with
            | [] -> raise Invalid_address
            | x :: xs -> (x, String.concat " " xs) in
        let has_zipcode address = String.ends_with address zipcode in
        let remove_zipcode address = String.sub address 0 (String.length address - String.length zipcode - 1) in
        let addresses = String.split_on_char ',' address_string in
        let relevant_addresses = List.(addresses |> filter has_zipcode |> map remove_zipcode) in
        match relevant_addresses with
            | [] -> String.concat "" [zipcode; ":/"]
            | _ ->
                let (house_numbers, streets) = List.(relevant_addresses |> map split_on_house_number |> split) in
                let suffix = String.concat "/" [String.concat "," streets; String.concat "," house_numbers] in
                String.concat ":" [zipcode; suffix]
