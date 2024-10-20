open Core
open Defs

type t = Card.t list

let rec make_cards_for_suite suite values cards =
  match values with
  | [] -> cards
  | value :: values ->
      make_cards_for_suite suite values (Card.make suite value :: cards)

let rec make_suites suites values cards =
  match suites with
  | [] -> cards
  | suite :: suites ->
      make_suites suites values (cards @ make_cards_for_suite suite values [])

let make =
  let suites = [ SClubs; SDiamonds; SHearts; SSpades ] in
  let values = [ Seven; Eight; Nine; Ten; Jack; Queen; King; Ace ] in
  make_suites suites values []

let shuffle deck =
  let new_deck = List.map ~f:(fun card -> (Random.bits (), card)) deck in
  let sorted_deck =
    List.sort new_deck ~compare:(fun (a, _) (b, _) -> if a < b then -1 else 1)
  in
  List.map sorted_deck ~f:(fun (_, card) -> card)

let print deck = List.iter ~f:(fun v -> print_endline @@ Card.show v) deck

let deal deck num_cards =
  let cards_in_deck = List.length deck in
  if num_cards < 0 || cards_in_deck < num_cards then
    Result.Error
      (Printf.sprintf "Can't deal %d cards. Number of cards in deck %d"
         num_cards cards_in_deck)
  else
    let rec deal_aux deck num_cards cards =
      match deck with
      | _ :: _ when num_cards = 0 -> Ok (cards, deck)
      | card :: deck -> deal_aux deck (num_cards - 1) (card :: cards)
      | [] -> Ok (cards, deck)
    in
    deal_aux deck num_cards []
