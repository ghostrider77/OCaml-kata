let mystery (n: int): int =
    n lxor (n lsr 1)


let mystery_inv (n: int): int =
    let rec loop k shifted =
        if shifted = 0 then k
        else loop (k lxor shifted) (shifted lsr 1)
    in loop n (n lsr 1)


let name_of_mystery (): string =
    "Gray code"
