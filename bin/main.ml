(* open Base;;*)
open Core

let () =
  (* randomizes random calls every run *)
  Random.self_init ();
  let game = Game.make in
  print_endline @@ Game.show game

(*let deck = Deck.make |> Deck.shuffle in*)
(*match Deck.deal deck 3 with*)
(*| Ok (hand, _) -> Deck.print hand*)
(*| Error msg -> print_endline msg*)
