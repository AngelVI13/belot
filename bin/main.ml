open Core
open Belot.Game

let () =
  let game = Belot.Game.make in
  Belot.Game.run_tests game
