let reverse_string (s: string): string =
    let n = String.length s in
    String.init n (fun ix -> s.[n - 1 - ix])
