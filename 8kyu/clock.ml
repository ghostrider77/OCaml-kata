let past (h: int) (m: int) (s: int): int =
    h |> ( * ) 60 |> (+) m |> ( * ) 60 |> (+) s |> ( * ) 1000
