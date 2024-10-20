open Core
open Defs
open Player

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
  { players; teams = make_teams; state = SShuffle; deck = Deck.make }

let show game = sprintf "game: %d" @@ List.length game.players

let finished game =
  match game.teams with
  | team1 :: team2 :: _ -> Team.points team1 > 141 || Team.points team2 > 141
  | _ -> failwithf "not enough teams %d" (List.length game.teams) ()

let do_shuffle game =
  { game with deck = Deck.shuffle game.deck; state = SDealPreBid }

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

let do_bidding game = { game with state = SDealRest }
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
