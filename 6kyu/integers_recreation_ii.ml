let prod2Sum (a: int) (b: int) (c: int) (d: int): int array array =
    let equal_pairs p1 p2 = (p1.(0) = p2.(0) && p1.(1) = p2.(1)) in
    let pair1 = [|abs (a*c - b*d); abs (a*d + b*c)|] in
    let pair2 = [|abs (a*d - b*c); abs (a*c + b*d)|] in
    Array.sort compare pair1;
    Array.sort compare pair2;
    if equal_pairs pair1 pair2 then [|pair1|]
    else if pair1.(0) < pair2.(0) then [|pair1; pair2|]
    else [|pair2; pair1|]
