open! Core
open Defs

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
  teams : Team.t list;
  state : game_state;
  chosen_game : cgame option;
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
    teams = make_teams;
    state = SShuffle;
    deck = Deck.make;
    chosen_game = None;
  }

let show game = sprintf "game: %d" @@ List.length game.players

let finished game =
  match game.teams with
  | team1 :: team2 :: _ -> Team.points team1 > 141 || Team.points team2 > 141
  | _ -> failwithf "not enough teams %d" (List.length game.teams) ()

let do_shuffle game =
  let players = List.map game.players ~f:(fun p -> Player.new_round p) in
  let deck = Deck.make |> Deck.shuffle in
  { game with deck; players; chosen_game = None; state = SDealPreBid }

let do_deal_pre_bid game =
  let rec deal players card_num deck new_players =
    match players with
    | [] -> (new_players, deck)
    | p :: players ->
        let cards, deck = Result.ok_or_failwith (Deck.deal deck card_num) in
        let player = Player.store_cards p (Deck.to_cards cards) in
        deal players card_num deck (player :: new_players)
  in

  (* first deal every player 3 cards then deal every player 2 cards *)
  let players, deck = deal game.players 3 game.deck [] in
  let players, deck = deal players 2 deck [] in
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

let best_bid cards =
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
  let best_score, best_game = List.nth_exn sorted_scores 0 in
  if Float.(best_score > 0.5) then Some best_game else None

let do_bidding game =
  (* TODO: bidding logic goes here *)
  let rec bid players current_bid bidder =
    let _ = (players, current_bid, bidder) in
    None
  in

  (* if no bid has been made -> go to next round *)
  let chosen_game = bid game.players None None in
  let state = match chosen_game with Some _ -> SDealRest | None -> SShuffle in

  { game with state; chosen_game }

let do_deal_rest game = { game with state = SPlay }
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

let fassert cond msg = if cond then () else failwith msg

let run_tests _game =
  let cards_score_spades_game =
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
    fassert (score = 19) "wrong score for a spades game"
  in
  printf "hello";
  cards_score_spades_game
