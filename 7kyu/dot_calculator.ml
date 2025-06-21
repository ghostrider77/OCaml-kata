let calculator (exp : string) : string =
  let left, op, right = Scanf.sscanf exp "%s %s %s" (fun a b c -> a, b, c) in
  let a = String.length left in
  let b = String.length right in
  let result = match op with
    | "+" -> a + b
    | "-" -> a - b
    | "*" -> a * b
    | "//" -> a / b
    | _ -> failwith "unknown operator" in
  String.make result '.'
