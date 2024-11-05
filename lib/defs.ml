type csuite = SClubs | SDiamonds | SHearts | SSpades [@@deriving show, enum]

type cgame = GClubs | GDiamonds | GHearts | GSpades | GNoTrumps | GAllTrumps
[@@deriving show, enum]

type ccounter = CNo | CCounter | CReCounter [@@deriving show]

type cvalue = Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
[@@deriving show, enum]

type ccombination =
  | Belot
  | Tierce of cvalue
  | Quarte of cvalue
  | Quinte of cvalue
  | Carre of cvalue
[@@deriving show]

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

let combination_worth = function
  | Belot -> 20
  | Tierce _ -> 20
  | Quarte _ -> 50
  | Quinte _ -> 100
  | Carre _ -> 100

type player_pos = North | West | South | East [@@deriving show, enum]
type player_type = Human | Machine [@@deriving show]

let partner = function
  | North -> South
  | West -> East
  | South -> North
  | East -> West

let five_cards_max_score = function
  (* J 9 A trumps +  A A non trumps = 67 *)
  | GClubs | GDiamonds | GHearts | GSpades ->
      trump_worth Jack + trump_worth Nine + (3 * no_trump_worth Ace)
  (* A A A A 10 = 54 *)
  | GNoTrumps -> (4 * no_trump_worth Ace) + no_trump_worth Ten
  (* J J J J 9 = 94 *)
  | GAllTrumps -> (4 * trump_worth Jack) + trump_worth Nine
