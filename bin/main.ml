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
  val deal : t -> int -> (t * t, string) result
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

  let rec deal deck num_cards =
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
  (* randomizes random calls every run *)
  Random.self_init ();
  let deck = Deck.make |> Deck.shuffle in
  match Deck.deal deck 3 with
  | Ok (hand, _) -> Deck.print hand
  | Error msg -> print_endline msg
