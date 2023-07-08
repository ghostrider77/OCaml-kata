let is_leap_year(year : int) : bool =
    if year mod 4 <> 0 then false
    else if year mod 400 = 0 then true
    else year mod 100 <> 0
