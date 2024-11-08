open! Core
open! Defs
open! Poly

(* TODO: read those from a config file *)
(* TODO: every round apply a small randomness to these numbers to imitate the
   feel of a real person playing. For example raiseNoBidScore = 0.50 + rand [0.2: 0.9].
   This should only be enabled during real game and should be disabled during testing.
*)
let raiseNoBidScore = 0.60
let raiseBidScore = 0.70
let counterBidScore = 0.75
let raisePartnerNoTrumpsScore = 0.85

(* TODO: make this a module ??? *)
type bid = { game : cgame; bidder : player_pos; counter : ccounter }
[@@deriving show]

let make_bid chosen_game bidder counter =
  { game = chosen_game; bidder; counter }

(* NOTE: these do not account for combination score *)
let color_game_score trump_suite card card_value =
  let is_trump = Card.is_trump card trump_suite in
  if is_trump then trump_worth card_value else no_trump_worth card_value

let no_trumps_score _ card_value = no_trump_worth card_value
let all_trumps_score _ card_value = trump_worth card_value

(* reduce score for naked tens or nines *)
let calc_card_score cards ~score_f =
  let rec count_score cards score =
    match cards with
    | card :: cards ->
        let card_value = Card.value card in
        let card_score = score_f card card_value in
        count_score cards (score + card_score)
    | [] -> score
  in
  count_score cards 0

let cards_score cards chosen_game =
  match chosen_game with
  | GSpades -> calc_card_score cards ~score_f:(color_game_score SSpades)
  | GDiamonds -> calc_card_score cards ~score_f:(color_game_score SDiamonds)
  | GHearts -> calc_card_score cards ~score_f:(color_game_score SHearts)
  | GClubs -> calc_card_score cards ~score_f:(color_game_score SClubs)
  | GNoTrumps -> calc_card_score cards ~score_f:no_trumps_score
  | GAllTrumps -> calc_card_score cards ~score_f:all_trumps_score

let calculate_best_game cards =
  assert (List.length cards = 5);

  let possible_games =
    [ GSpades; GDiamonds; GHearts; GClubs; GNoTrumps; GAllTrumps ]
  in
  let game_scores =
    List.map possible_games ~f:(fun game ->
        ( float_of_int (cards_score cards game)
          /. float_of_int (five_cards_max_score game),
          game ))
  in
  (* sort in descending order *)
  let sorted_scores =
    List.sort game_scores ~compare:(fun (score1, _) (score2, _) ->
        if Float.(score1 < score2) then 1 else -1)
  in
  List.take sorted_scores 3

let decide_bid current_bid best_score best_game player_pos =
  match current_bid with
  | None ->
      if Float.(best_score > raiseNoBidScore) then
        Some (make_bid best_game player_pos CNo)
      else None
  | Some current_b ->
      (* my best game is same as current bid (from my partner) -> do nothing *)
      if best_game = current_b.game && player_pos = partner current_b.bidder
      then None
      else if
        (* my best game is same as current bid (from opponent) -> raise counter
           if cards worth is > X% *)
        best_game = current_b.game
        && player_pos <> partner current_b.bidder
        && Float.(best_score > counterBidScore)
      then
        match current_b.counter with
        | CNo -> Some (make_bid best_game player_pos CCounter)
        | CCounter -> Some (make_bid best_game player_pos CReCounter)
        | CReCounter -> None
      else if cgame_to_enum best_game > cgame_to_enum current_b.game then
        (* my best game is bigger than the current bid -> raise bid *)
        if
          (* if game is no trumps and current bidder is my partner do not
             raise to all trumps if cards worth less than <X% *)
          player_pos = partner current_b.bidder
          && current_b.game = GNoTrumps
          && Float.(best_score <= raisePartnerNoTrumpsScore)
        then None
        else if Float.(best_score > raiseBidScore) then
          (* make bid only if my cards are worth more than >X% *)
          Some (make_bid best_game player_pos CNo)
        else None
      else if
        (* my best game is smaller than the current bid (from partner) and
           current bid is a color and cards are worth >X% -> raise to AllTrumps *)
        cgame_to_enum best_game < cgame_to_enum current_b.game
        && player_pos = partner current_b.bidder
        && cgame_to_enum current_b.game < cgame_to_enum GNoTrumps
        && Float.(best_score > raiseBidScore)
      then Some (make_bid GAllTrumps player_pos CNo)
      else None

let best_bid cards current_bid player_pos =
  let top_games = calculate_best_game cards in
  printf "%s\n" @@ show_player_pos player_pos;
  List.iter top_games ~f:(fun (s, g) -> printf "%f %s\n" s (show_cgame g));
  printf "---------\n";

  (* if current top bid is for example AllTrumps but your best game is Clubs
      then try out your next best games because maybe you have a high enough
      score for AllTrumps that is strong enough to raise the current bid *)
  let rec decide candidates =
    match candidates with
    | [] -> None
    | (candidate_score, candidate_game) :: tl -> (
        let bid =
          decide_bid current_bid candidate_score candidate_game player_pos
        in
        match bid with None -> decide tl | Some _ -> bid)
  in

  decide top_games
(* TODO: if you bid color and then your partner bids another color -> bid all trumps *)

let%expect_test "test_cards_score" =
  let cards =
    [
      Card.make SSpades Jack;
      Card.make SSpades Nine;
      Card.make SSpades Ace;
      Card.make SHearts Nine;
      Card.make SHearts Jack;
    ]
  in

  let score = cards_score cards GSpades in
  printf "%d score for a spades game" score;
  [%expect {| 47 score for a spades game |}];
  let score = cards_score cards GDiamonds in
  printf "%d score for a diamonds game" score;
  [%expect {| 15 score for a diamonds game |}];
  let score = cards_score cards GNoTrumps in
  printf "%d score for a no trumps game" score;
  [%expect {| 15 score for a no trumps game |}];
  let score = cards_score cards GAllTrumps in
  printf "%d score for a all trumps game" score;
  [%expect {| 79 score for a all trumps game |}]

let%expect_test "test_best_bid_all_trumps" =
  let cards =
    [
      Card.make SSpades Jack;
      Card.make SSpades Nine;
      Card.make SSpades Ace;
      Card.make SHearts Nine;
      Card.make SHearts Jack;
    ]
  in
  let bid = best_bid cards None South in
  match bid with
  | None -> [%expect.unreachable]
  | Some b ->
      printf "%s" @@ show_cgame b.game;
      [%expect
        {|
        Defs.South
        0.840426 Defs.GAllTrumps
        0.701493 Defs.GSpades
        0.701493 Defs.GHearts
        ---------
        Defs.GAllTrumps
        |}];
      printf "%s" @@ show_ccounter b.counter;
      [%expect {| Defs.CNo |}]

let%expect_test "test_best_bid_all_trumps_counter" =
  let cards =
    [
      Card.make SSpades Jack;
      Card.make SSpades Nine;
      Card.make SSpades Ace;
      Card.make SHearts Nine;
      Card.make SHearts Jack;
    ]
  in
  let current_bid = Some { game = GAllTrumps; bidder = East; counter = CNo } in

  let bid = best_bid cards current_bid South in
  match bid with
  | None -> [%expect.unreachable]
  | Some b ->
      printf "%s" @@ show_cgame b.game;
      [%expect
        {|
        Defs.South
        0.840426 Defs.GAllTrumps
        0.701493 Defs.GSpades
        0.701493 Defs.GHearts
        ---------
        Defs.GAllTrumps
        |}];
      printf "%s" @@ show_ccounter b.counter;
      [%expect {| Defs.CCounter |}]

let%expect_test "test_best_bid_all_trumps_recounter" =
  let cards =
    [
      Card.make SSpades Jack;
      Card.make SSpades Nine;
      Card.make SSpades Ace;
      Card.make SHearts Nine;
      Card.make SHearts Jack;
    ]
  in
  let current_bid =
    Some { game = GAllTrumps; bidder = East; counter = CCounter }
  in

  let bid = best_bid cards current_bid South in
  match bid with
  | None -> [%expect.unreachable]
  | Some b ->
      printf "%s" @@ show_cgame b.game;
      [%expect
        {|
        Defs.South
        0.840426 Defs.GAllTrumps
        0.701493 Defs.GSpades
        0.701493 Defs.GHearts
        ---------
        Defs.GAllTrumps
        |}];
      printf "%s" @@ show_ccounter b.counter;
      [%expect {| Defs.CReCounter |}]

let%expect_test "test_best_bid_no_trumps" =
  let cards =
    [
      Card.make SDiamonds Ace;
      Card.make SSpades Ten;
      Card.make SSpades Ace;
      Card.make SClubs King;
      Card.make SClubs Queen;
    ]
  in
  let bid = best_bid cards None South in
  match bid with
  | None -> [%expect.unreachable]
  | Some b ->
      printf "%s" @@ show_cgame b.game;
      [%expect
        {|
        Defs.South
        0.722222 Defs.GNoTrumps
        0.582090 Defs.GSpades
        0.582090 Defs.GDiamonds
        ---------
        Defs.GNoTrumps
        |}];
      printf "%s" @@ show_ccounter b.counter;
      [%expect {| Defs.CNo |}]

let%expect_test "test_best_bid_color" =
  let cards =
    [
      Card.make SDiamonds King;
      Card.make SSpades Ten;
      Card.make SSpades Ace;
      Card.make SClubs Jack;
      Card.make SClubs Queen;
    ]
  in
  let bid = best_bid cards None South in
  match bid with
  | None -> [%expect.unreachable]
  | Some b ->
      printf "%s" @@ show_cgame b.game;
      [%expect
        {|
        Defs.South
        0.716418 Defs.GClubs
        0.555556 Defs.GNoTrumps
        0.510638 Defs.GAllTrumps
        ---------
        Defs.GClubs
        |}];
      printf "%s" @@ show_ccounter b.counter;
      [%expect {| Defs.CNo |}]
