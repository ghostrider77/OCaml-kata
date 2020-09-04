let evaporator _ (evapPerDay: float) (threshold: float): int =
    (log (threshold /. 100.0)) /. (log (1.0 -. evapPerDay /. 100.0)) |> ceil |> int_of_float
