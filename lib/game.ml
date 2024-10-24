open! Core
open Defs
open! Poly

type game_state =
  | SShuffle
  | SDealPreBid
  | SBidding
  | SDealRest
  | SPlay (* TODO: does Announce need its own state *)
  | SCalcScore
[@@deriving show, enum]

type t = {
  players : Player.t list;
  dealer_idx : int;
  teams : Team.t list;
  state : game_state;
  chosen_game : cgame option;
  bidder : Player.t option;
  counter : ccounter;
  deck : Deck.t;
}

let make_players =
  let rec make_players player_positions players =
    match player_positions with
    | pos :: player_positions ->
        make_players player_positions (Player.make pos :: players)
    | [] -> players
  in
  make_players [ South; East; North; West ] []

let make_teams =
  let team1 = Team.make (North, South) in
  let team2 = Team.make (East, West) in
  [ team1; team2 ]

let make =
  let players = make_players in
  {
    players;
    dealer_idx = 0;
    teams = make_teams;
    state = SShuffle;
    deck = Deck.make;
    chosen_game = None;
    bidder = None;
    counter = CNo;
  }

let show game = sprintf "game: %d" @@ List.length game.players

let finished game =
  match game.teams with
  | team1 :: team2 :: _ -> Team.points team1 > 141 || Team.points team2 > 141
  | _ -> failwithf "not enough teams %d" (List.length game.teams) ()

let next_player_idx idx = (idx + 1) % 4

let do_shuffle game =
  let players = List.map game.players ~f:(fun p -> Player.new_round p) in
  let dealer_idx = next_player_idx game.dealer_idx in
  let deck = Deck.make |> Deck.shuffle in
  {
    game with
    deck;
    players;
    dealer_idx;
    chosen_game = None;
    bidder = None;
    state = SDealPreBid;
    counter = CNo;
  }

let rec deal_cards players card_num deck new_players =
  match players with
  | [] -> (new_players, deck)
  | p :: players ->
      let cards, deck = Result.ok_or_failwith (Deck.deal deck card_num) in
      let player = Player.store_cards p (Deck.to_cards cards) in
      deal_cards players card_num deck (player :: new_players)

let do_deal_pre_bid game =
  (* first deal every player 3 cards then deal every player 2 cards *)
  let players, deck = deal_cards game.players 3 game.deck [] in
  let players, deck = deal_cards players 2 deck [] in
  { game with state = SBidding; players; deck }

(* NOTE: these do not account for combination score *)
let color_game_score trump_suite card card_value =
  let is_trump = Card.is_trump card trump_suite in
  if is_trump then trump_worth card_value else no_trump_worth card_value

let no_trumps_score _ card_value = no_trump_worth card_value
let all_trumps_score _ card_value = trump_worth card_value

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

let best_bid cards current_bid current_counter =
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
  (*List.iter sorted_scores ~f:(fun (s, g) -> printf "%f %s\n" s (show_cgame g));*)
  let best_score, best_game = List.nth_exn sorted_scores 0 in
  match current_bid with
  | None ->
      if Float.(best_score > 0.5) then (Some best_game, CNo) else (None, CNo)
  | Some current_b ->
      if best_game = current_b && Float.(best_score > 0.75) then
        match current_counter with
        | CNo -> (Some best_game, CCounter)
        | CCounter -> (Some best_game, CReCounter)
        | CReCounter -> (None, CReCounter)
      else if cgame_to_enum best_game > cgame_to_enum current_b then
        (Some best_game, CNo)
      else (None, CNo)

(* TODO: write tests for this function!!! *)
let do_bidding game =
  let rec bid player_idx current_bid bidder counter num_passes =
    if (Option.is_some current_bid && num_passes = 3) || num_passes = 4 then
      (current_bid, bidder, counter)
    else
      let player = List.nth_exn game.players player_idx in
      let player_bid, counter =
        best_bid (Player.cards player) current_bid counter
      in
      match player_bid with
      | None -> bid (next_player_idx player_idx) None None CNo (num_passes + 1)
      | Some b ->
          bid (next_player_idx player_idx) current_bid (Some player) counter 0
  in

  (* start bidding from the person east of the dealer *)
  let chosen_game, bidder, counter =
    bid (next_player_idx game.dealer_idx) None None CNo 0
  in

  (* if no bid has been made -> go to next round *)
  let state = match chosen_game with Some _ -> SDealRest | None -> SShuffle in

  { game with state; chosen_game; bidder; counter }

let do_deal_rest game =
  let players, deck = deal_cards game.players 3 game.deck [] in
  { game with state = SPlay; deck; players }

(* TODO: implement these *)
let do_play game = { game with state = SCalcScore }
let do_calc_score game = { game with state = SShuffle }

let rec play game =
  if finished game then game
  else
    let game =
      match game.state with
      | SShuffle -> do_shuffle game
      | SDealPreBid -> do_deal_pre_bid game
      | SBidding -> do_bidding game
      | SDealRest -> do_deal_rest game
      | SPlay -> do_play game
      | SCalcScore -> do_calc_score game
    in
    play game

(* improve this assert to be actually useful to print left and right values *)
let fassert cond msg = if cond then () else failwith msg

let test_cards_score =
  let cards =
    [
      Card.make SSpades Jack;
      Card.make SSpades Nine;
      Card.make SSpades Ace;
      Card.make SHearts Nine;
      Card.make SHearts Jack;
    ]
  in

  let test_cards_score_spades_game =
    let score = cards_score cards GSpades in
    fassert (score = 47)
      (sprintf "%d = 47: wrong score for a spades game" score)
  in
  let test_cards_score_diamonds_game =
    let score = cards_score cards GDiamonds in
    fassert (score = 15)
      (sprintf "%d = 47: wrong score for a spades game" score)
  in
  let test_cards_score_no_trumps_game =
    let score = cards_score cards GNoTrumps in
    fassert (score = 15)
      (sprintf "%d = 15: wrong score for a no trumps game" score)
  in
  let test_cards_score_all_trumps_game =
    let score = cards_score cards GAllTrumps in
    fassert (score = 79)
      (sprintf "%d = 79: wrong score for a all trumps game" score)
  in
  test_cards_score_spades_game;
  test_cards_score_diamonds_game;
  test_cards_score_no_trumps_game;
  test_cards_score_all_trumps_game

let test_best_bid =
  let test_best_bid_all_trumps =
    let cards =
      [
        Card.make SSpades Jack;
        Card.make SSpades Nine;
        Card.make SSpades Ace;
        Card.make SHearts Nine;
        Card.make SHearts Jack;
      ]
    in
    match best_bid cards None CNo with
    | Some g, counter ->
        fassert (Poly.( = ) g GAllTrumps)
          (sprintf "%s: wrong bid test_best_bid_all_trumps" @@ show_cgame g);
        fassert (Poly.( = ) counter CNo)
          (sprintf "%s: wrong counter test_best_bid_all_trumps" @@ show_cgame g)
    | None, counter -> fassert false "no bid test_best_bid_all_trumps"
  in
  let test_best_bid_all_trumps_counter =
    let cards =
      [
        Card.make SSpades Jack;
        Card.make SSpades Nine;
        Card.make SSpades Ace;
        Card.make SHearts Nine;
        Card.make SHearts Jack;
      ]
    in
    match best_bid cards (Some GAllTrumps) CNo with
    | Some g, counter ->
        fassert (Poly.( = ) g GAllTrumps)
          (sprintf "%s: wrong bid test_best_bid_all_trumps" @@ show_cgame g);
        fassert
          (Poly.( = ) counter CCounter)
          (sprintf "%s: wrong counter test_best_bid_all_trumps" @@ show_cgame g)
    | None, counter -> fassert false "no bid test_best_bid_all_trumps"
  in
  let test_best_bid_all_trumps_recounter =
    let cards =
      [
        Card.make SSpades Jack;
        Card.make SSpades Nine;
        Card.make SSpades Ace;
        Card.make SHearts Nine;
        Card.make SHearts Jack;
      ]
    in
    match best_bid cards (Some GAllTrumps) CCounter with
    | Some g, counter ->
        fassert (Poly.( = ) g GAllTrumps)
          (sprintf "%s: wrong bid test_best_bid_all_trumps" @@ show_cgame g);
        fassert
          (Poly.( = ) counter CReCounter)
          (sprintf "%s: wrong counter test_best_bid_all_trumps" @@ show_cgame g)
    | None, counter -> fassert false "no bid test_best_bid_all_trumps"
  in
  let test_best_bid_no_trumps =
    let cards =
      [
        Card.make SDiamonds King;
        Card.make SSpades Ten;
        Card.make SSpades Ace;
        Card.make SClubs King;
        Card.make SClubs Queen;
      ]
    in
    match best_bid cards None CNo with
    | Some g, _ ->
        fassert (Poly.( = ) g GNoTrumps)
          (sprintf "%s: wrong bid test_best_bid_no_trumps" @@ show_cgame g)
    | None, _ -> fassert false "no bid test_best_bid_no_trumps"
  in
  let test_best_bid_color =
    let cards =
      [
        Card.make SDiamonds King;
        Card.make SSpades Ten;
        Card.make SSpades Ace;
        Card.make SClubs Jack;
        Card.make SClubs Queen;
      ]
    in
    match best_bid cards None CNo with
    | Some g, _ ->
        fassert (Poly.( = ) g GClubs)
          (sprintf "%s: wrong bid test_best_bid_color" @@ show_cgame g)
    | None, _ -> fassert false "no bid test_best_bid_color"
  in
  test_best_bid_no_trumps;
  test_best_bid_all_trumps;
  test_best_bid_all_trumps_counter;
  test_best_bid_all_trumps_recounter;
  test_best_bid_color

let run_tests _game =
  test_cards_score;
  test_best_bid

let%expect_test "cards_score:spades_game" =
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
  printf "---%d---" score;
  [%expect {| |}]
