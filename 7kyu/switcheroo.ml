let switcheroo (s: string): string =
    let replace = function
        | 'a' -> 'b'
        | 'b' -> 'a'
        | chr -> chr in
    String.map replace s
