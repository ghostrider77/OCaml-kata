let bouncing_ball (h: float) (bounce: float) (window: float) =
    let is_valid = h > 0.0 && 0.0 < bounce && bounce < 1.0 && window < h in
    let rec calc_nr_passes height acc =
        let height' = height *. bounce in
        if height' > window then calc_nr_passes height' (acc + 2) else acc + 1 in
    if is_valid then calc_nr_passes h 0 else -1
