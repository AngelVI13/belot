open! Core
open! Belot.Game

let () =
  let game = Belot.Game.make in
  print_endline @@ Belot.Game.show game
