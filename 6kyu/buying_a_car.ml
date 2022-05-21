let nb_months (start_price_old: float) (start_price_new: float) (saving_per_month: float) (percent_loss: float) =
    let round x = int_of_float (x +. 0.5) in
    let rec loop car_price_diff nr_months percent =
        let p = if nr_months mod 2 = 0 then percent +. 0.5 else percent in
        let diff = car_price_diff *. (1.0 -. p /. 100.0) in
        let total = diff +. (float nr_months) *. saving_per_month in
        if total >= 0.0 then (nr_months, round total)
        else loop diff (nr_months + 1) p in
    if start_price_old >= start_price_new then (0, round (start_price_old -. start_price_new))
    else loop (start_price_old -. start_price_new) 1 percent_loss
