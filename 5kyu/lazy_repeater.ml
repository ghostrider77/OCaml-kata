let make_looper(s : string) : (unit -> char) =
  let chars = s |> String.to_seq |> Seq.cycle |> Seq.to_dispenser in
  fun () -> Option.get @@ chars ()
