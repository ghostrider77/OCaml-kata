type rectangle = {top : int; right : int; bottom : int; left : int}


let will_robots_collide (x1: int) (y1: int) (x2: int) (y2: int) (cmds: char list): bool =
  let move ({top; right; bottom; left} as rectangle) = function
    | 'U' -> {rectangle with top = top + 1}
    | 'R' -> {rectangle with right = right + 1}
    | 'D' -> {rectangle with bottom = bottom - 1}
    | 'L' -> {rectangle with left = left - 1}
    | _ -> failwith "Unknown direction." in
  let calc_trajectory x0 y0 =
    List.fold_left move {top = y0; right = x0; bottom = y0; left = x0} cmds in
  let does_intersect r1 r2 =
    let intersect_x = r1.right >= r2.left && r2.right >= r1.left in
    let intersect_y = r1.top >= r2.bottom && r2.top >= r1.bottom in
    intersect_x && intersect_y in
  let first = calc_trajectory x1 y1 in
  let second = calc_trajectory x2 y2 in
  does_intersect first second
