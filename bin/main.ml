(* open Base;;*)

type csuite =
    | Clubs
    | Diamonds
    | Hearts
    | Spades

type cgame =
    | Clubs
    | Diamonds
    | Hearts
    | Spades
    | NoTrumps
    | AllTrumps

type cvalue =
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace

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



type card = { suite: csuite; value: cvalue }
[@@deriving show]

let rec make_cards_for_suite suite values cards = 
    match values with
    | [] -> cards
    | value :: values -> make_cards_for_suite suite values ({ suite; value } :: cards)

let rec make_suites suites values cards = 
    match suites with
    | [] -> cards
    | suite :: suites -> make_suites suites values (cards @ make_cards_for_suite suite values [])

let make_deck =
    (* TODO: each type has to be unique and Clubs appears in both csuites and cgames *)
    let suites = [Clubs; Diamonds; Hearts; Spades] in
    let values = [Seven; Eight; Nine; Ten; Jack; Queen; King; Ace] in
    make_suites suites values []

let () = print_endline "Hello, World!"
