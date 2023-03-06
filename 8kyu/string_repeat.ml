let string_repeat (s : string) (n : int) : string =
    String.concat "" @@ List.init n (fun _ -> s)
