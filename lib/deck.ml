open Core
open Defs
open! Cards

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
  assert (num_cards > 0 && cards_in_deck >= num_cards);

  List.split_n deck num_cards

let to_cards deck = deck
let of_cards cards = cards

(* NOTE: this is only here so i can copy generated cards *)
let%expect_test "deck make->shuffle" =
  let cards = make |> shuffle |> to_cards in
  printf "%s" (show_card_list cards);
  [%expect {|
    { Card.suite = Defs.SDiamonds; value = Defs.Eight }
    { Card.suite = Defs.SDiamonds; value = Defs.Seven }
    { Card.suite = Defs.SClubs; value = Defs.Queen }
    { Card.suite = Defs.SDiamonds; value = Defs.Jack }
    { Card.suite = Defs.SClubs; value = Defs.Ten }
    { Card.suite = Defs.SSpades; value = Defs.Ten }
    { Card.suite = Defs.SHearts; value = Defs.Ten }
    { Card.suite = Defs.SHearts; value = Defs.King }
    { Card.suite = Defs.SHearts; value = Defs.Jack }
    { Card.suite = Defs.SSpades; value = Defs.Ace }
    { Card.suite = Defs.SClubs; value = Defs.Ace }
    { Card.suite = Defs.SSpades; value = Defs.Jack }
    { Card.suite = Defs.SClubs; value = Defs.Seven }
    { Card.suite = Defs.SClubs; value = Defs.Eight }
    { Card.suite = Defs.SDiamonds; value = Defs.King }
    { Card.suite = Defs.SHearts; value = Defs.Queen }
    { Card.suite = Defs.SHearts; value = Defs.Ace }
    { Card.suite = Defs.SSpades; value = Defs.Eight }
    { Card.suite = Defs.SClubs; value = Defs.King }
    { Card.suite = Defs.SSpades; value = Defs.King }
    { Card.suite = Defs.SDiamonds; value = Defs.Nine }
    { Card.suite = Defs.SHearts; value = Defs.Seven }
    { Card.suite = Defs.SHearts; value = Defs.Nine }
    { Card.suite = Defs.SSpades; value = Defs.Queen }
    { Card.suite = Defs.SClubs; value = Defs.Nine }
    { Card.suite = Defs.SClubs; value = Defs.Jack }
    { Card.suite = Defs.SHearts; value = Defs.Eight }
    { Card.suite = Defs.SDiamonds; value = Defs.Ten }
    { Card.suite = Defs.SSpades; value = Defs.Nine }
    { Card.suite = Defs.SDiamonds; value = Defs.Ace }
    { Card.suite = Defs.SDiamonds; value = Defs.Queen }
    { Card.suite = Defs.SSpades; value = Defs.Seven }
    |}]
