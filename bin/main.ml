open! Core
open! Belot.Game
open! Belot.Deck
open! Belot.Deck

let gen_cards _ =
  let cards = Belot.Deck.make |> Belot.Deck.shuffle |> Belot.Deck.to_cards in
  let cards = List.take cards 20 in
  List.iter cards ~f:(fun c -> printf "%s\n" @@ Belot.Card.show c)

let () =
  let game = Belot.Game.make in
  gen_cards game
(*print_endline @@ Belot.Game.show game*)
