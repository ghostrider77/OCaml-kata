type roll = Nun | Gimel | Hei | Shin
type state = {account : int; pot : int}

let play_one_round ({account; pot} as state) = function
  | Nun -> state
  | Gimel -> {account = account + pot; pot = 0}
  | Hei -> {account = account + pot / 2; pot = pot - pot / 2}
  | Shin -> {account = account - 1; pot = pot + 1}


let gamble (rolls: roll list) (my_coins: int) (pot: int): int =
  let rec loop ({account; pot} as st) = function
    | [] -> account
    | r :: rs ->
      let st' = play_one_round st r in
      loop st' rs
  in loop {account = my_coins; pot} rolls
