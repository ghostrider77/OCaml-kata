type fb =
    | Num of int
    | Fizz
    | Buzz
    | FizzBuzz


let fibs_fizz_buzz (n: int): fb list =
    let rec loop acc a b k =
        if k >= n then List.rev acc
        else loop (a :: acc) b (a + b) (k + 1) in
    let replace f =
        if f mod 3 = 0 && f mod 5 = 0 then FizzBuzz
        else if f mod 3 = 0 then Fizz
        else if f mod 5 = 0 then Buzz
        else Num f in
    let fibs = loop [] 1 1 0 in
    List.map replace fibs
