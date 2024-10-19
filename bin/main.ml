(* open Base;;*)
open Core

type csuite = SClubs | SDiamonds | SHearts | SSpades [@@deriving show]

type cgame = GClubs | GDiamonds | GHearts | GSpades | GNoTrumps | GAllTrumps
[@@deriving show, enum]

type cvalue = Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
[@@deriving show]

type ccombination =
  | Belot
  | Tierce
  | Quarte
  | Quinte
  | Carre
  | Carre9s
  | CarreJs

(* TODO: these should be maps? *)
let trump_order = function
  | Seven -> 1
  | Eight -> 2
  | Queen -> 3
  | King -> 4
  | Ten -> 5
  | Ace -> 6
  | Nine -> 7
  | Jack -> 8

let trump_worth = function
  | Seven -> 0
  | Eight -> 0
  | Queen -> 3
  | King -> 4
  | Ten -> 10
  | Ace -> 11
  | Nine -> 14
  | Jack -> 20

let no_trump_order = function
  | Seven -> 1
  | Eight -> 2
  | Nine -> 3
  | Jack -> 4
  | Queen -> 5
  | King -> 6
  | Ten -> 7
  | Ace -> 8

let no_trump_worth = function
  | Seven -> 0
  | Eight -> 0
  | Nine -> 0
  | Jack -> 2
  | Queen -> 3
  | King -> 4
  | Ten -> 10
  | Ace -> 11

let combination_order = function
  | Seven -> 1
  | Eight -> 2
  | Nine -> 3
  | Ten -> 4
  | Jack -> 5
  | Queen -> 6
  | King -> 7
  | Ace -> 8

let combination_worth = function
  | Belot -> 20
  | Tierce -> 20
  | Quarte -> 50
  | Quinte -> 100
  | Carre -> 100
  | Carre9s -> 150
  | CarreJs -> 200

type card = { suite : csuite; value : cvalue } [@@deriving show]

let rec make_cards_for_suite suite values cards =
  match values with
  | [] -> cards
  | value :: values ->
      make_cards_for_suite suite values ({ suite; value } :: cards)

let rec make_suites suites values cards =
  match suites with
  | [] -> cards
  | suite :: suites ->
      make_suites suites values (cards @ make_cards_for_suite suite values [])

module Deck : sig
  type t

  val make : t
  val shuffle : t -> t
  val print : t -> unit
end = struct
  type t = card list

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

  let print deck = List.iter ~f:(fun v -> print_endline @@ show_card v) deck
end

type team = One | Two [@@deriving show, enum]

type player = {
  name : string;
  cards : card list;
  announce : cgame option;
  points : int;
  team : int;
}
[@@deriving show]

let () =
  Random.self_init ();
  (* randomizes random calls every run *)
  let deck = Deck.make |> Deck.shuffle in
  Deck.print deck
