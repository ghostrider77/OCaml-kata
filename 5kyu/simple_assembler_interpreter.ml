module Register = Map.Make(String)


type content = Constant of int | RegisterName of string


type instruction =
    | Move of content * content
    | Increment of content
    | Decrement of content
    | ConditionalJump of content * content


let alphabet =
    let rec loop acc code =
        let char = Char.chr code in
        if char = 'z' then List.rev_map (String.make 1) (char :: acc)
        else loop (char :: acc) (code + 1)
    in loop [] 97


let content_of_string s =
    if List.mem s alphabet then RegisterName s
    else Constant (int_of_string s)


let instruction_of_string s =
    let convert_to_string_list s =
        Str.(s |> split (regexp " ")) in
    match convert_to_string_list s with
        | "mov" :: x :: y :: _ -> Move (content_of_string x, content_of_string y)
        | "inc" :: x :: _ -> Increment (content_of_string x)
        | "dec" :: x :: _ -> Decrement (content_of_string x)
        | "jnz" :: x :: y :: _ -> ConditionalJump (content_of_string x, content_of_string y)
        | _ -> failwith "Syntactically incorrect expression."


let simple_assembler (program : string list): ((string * int) list) =
    let instructions = program |> List.map instruction_of_string |> Array.of_list in
    let nr_instructions = Array.length instructions in
    let extract_value registers = function
        | Constant v -> v
        | RegisterName address -> Register.find address registers in
    let rec loop registers position =
        if position >= nr_instructions then Register.bindings registers
        else match instructions.(position) with
            | Move (RegisterName address, content) ->
                let v = extract_value registers content in
                loop (Register.add address v registers) (position + 1)
            | Increment (RegisterName address) ->
                loop (Register.add address ((Register.find address registers) + 1) registers) (position + 1)
            | Decrement (RegisterName address) ->
                loop (Register.add address ((Register.find address registers) - 1) registers) (position + 1)
            | ConditionalJump (condition, jump) ->
                let condition_value = extract_value registers condition in
                if condition_value = 0 then loop registers (position + 1)
                else loop registers (position + extract_value registers jump)
            | _ -> loop registers (position + 1) in
    loop Register.empty 0
