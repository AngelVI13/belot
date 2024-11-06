open! Core
open Defs
open! Poly
open! Cards
open! Bid

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
  chosen_game : bid option;
  bid_history : (player_pos * bid option) list; (* this is for debug only *)
  deck : Deck.t;
}

let make_players =
  let rec make_players player_positions players =
    match player_positions with
    | pos :: player_positions ->
        make_players player_positions (Player.make pos :: players)
    | [] -> List.rev players
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
    (* this gets incremented in shuffle *)
    dealer_idx = -1;
    teams = make_teams;
    state = SShuffle;
    deck = Deck.make;
    chosen_game = None;
    bid_history = [];
  }

let show (game : t) =
  let chosen_game_s, bidder_s, counter_s =
    match game.chosen_game with
    | None -> ("no", "no", "no")
    | Some s ->
        (show_cgame s.game, show_player_pos s.bidder, show_ccounter s.counter)
  in
  let bid_history =
    List.map game.bid_history ~f:(fun (pos, bid) ->
        match bid with
        | None -> sprintf "%s pass\n" (show_player_pos pos)
        | Some b -> sprintf "%s %s\n" (show_player_pos pos) (show_bid b))
  in
  let players = List.map game.players ~f:(fun p -> Player.show p) in
  sprintf "State: %s\nGame: %s %s %s\n---\nBid History:\n%s---\nPlayers:\n%s"
    (show_game_state game.state)
    counter_s chosen_game_s bidder_s
    (List.fold bid_history ~init:"" ~f:(fun acc el -> sprintf "%s%s" acc el))
    (List.fold players ~init:"" ~f:(fun acc el -> sprintf "%s%s" acc el))

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
    state = SDealPreBid;
  }

let rec deal_cards players card_num deck new_players =
  match players with
  | [] -> (List.rev new_players, deck)
  | p :: players ->
      let cards, deck = Deck.deal deck card_num in
      let player = Player.store_cards p (Deck.to_cards cards) in
      deal_cards players card_num deck (player :: new_players)

let do_deal_pre_bid game =
  (* first deal every player 3 cards then deal every player 2 cards *)
  let players, deck = deal_cards game.players 3 game.deck [] in
  let players, deck = deal_cards players 2 deck [] in
  { game with state = SBidding; players; deck }

let do_bidding game =
  let rec bid player_idx current_bid num_passes history =
    if (Option.is_some current_bid && num_passes = 3) || num_passes = 4 then
      (current_bid, List.rev history)
    else
      let player = List.nth_exn game.players player_idx in
      let player_pos = Player.pos player in
      let player_bid = best_bid (Player.cards player) current_bid player_pos in
      (*printf "%b %d %s %d"*)
      (*  (Option.is_some player_bid)*)
      (*  player_idx (show_ccounter counter) num_passes;*)
      let next_player = next_player_idx player_idx in
      match player_bid with
      (* None is considered a pass -> pass on the current bid and current counter *)
      | None ->
          bid next_player current_bid (num_passes + 1)
            ((player_pos, None) :: history)
      (* Some means the player raised the current bid or counter *)
      | Some _ ->
          bid next_player player_bid 0 ((player_pos, player_bid) :: history)
  in

  (* start bidding from the person east of the dealer *)
  let final_bid, bid_history =
    bid (next_player_idx game.dealer_idx) None 0 []
  in

  (* if no bid has been made -> go to next round *)
  let state = match final_bid with Some _ -> SDealRest | None -> SShuffle in
  { game with state; chosen_game = final_bid; bid_history }

let do_deal_rest game =
  let players, deck = deal_cards game.players 3 game.deck [] in
  { game with state = SPlay; deck; players }

let announce_combination player =
  let cards = Player.cards player in
  let combos = find_best_combination cards in
  (Player.store_combos player combos, combos)

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

let test_setup_do_bidding deck =
  let game = make |> do_shuffle in
  let players, deck = deal_cards game.players 5 (Deck.of_cards deck) [] in
  let game = { game with state = SBidding; players; deck } in
  let game = do_bidding game in
  game

let%expect_test "do_bidding_1" =
  let test_deck =
    [
      (* south *)
      Card.make SSpades Eight;
      Card.make SSpades Ten;
      Card.make SSpades Ace;
      Card.make SHearts Eight;
      Card.make SSpades Nine;
      (* east *)
      Card.make SSpades Jack;
      Card.make SClubs Jack;
      Card.make SDiamonds King;
      Card.make SDiamonds Ace;
      Card.make SClubs Nine;
      (* north *)
      Card.make SHearts Ten;
      Card.make SHearts Ace;
      Card.make SHearts Jack;
      Card.make SDiamonds Ten;
      Card.make SClubs Eight;
      (* west *)
      Card.make SDiamonds Eight;
      Card.make SSpades Seven;
      Card.make SDiamonds Queen;
      Card.make SHearts Seven;
      Card.make SClubs King;
    ]
  in
  let game = test_setup_do_bidding test_deck in

  List.iter game.players ~f:(fun p ->
      printf "%s " @@ show_player_pos @@ Player.pos p);
  [%expect
    {|
    Defs.East
    0.761194 Defs.GClubs
    0.734043 Defs.GAllTrumps
    0.552239 Defs.GSpades
    ---------
    Defs.North
    0.761194 Defs.GHearts
    0.611111 Defs.GNoTrumps
    0.542553 Defs.GAllTrumps
    ---------
    Defs.West
    0.129630 Defs.GNoTrumps
    0.104478 Defs.GSpades
    0.104478 Defs.GDiamonds
    ---------
    Defs.South
    0.522388 Defs.GSpades
    0.388889 Defs.GNoTrumps
    0.372340 Defs.GAllTrumps
    ---------
    Defs.East
    0.761194 Defs.GClubs
    0.734043 Defs.GAllTrumps
    0.552239 Defs.GSpades
    ---------
    Defs.North
    0.761194 Defs.GHearts
    0.611111 Defs.GNoTrumps
    0.542553 Defs.GAllTrumps
    ---------
    Defs.West
    0.129630 Defs.GNoTrumps
    0.104478 Defs.GSpades
    0.104478 Defs.GDiamonds
    ---------
    Defs.South
    0.522388 Defs.GSpades
    0.388889 Defs.GNoTrumps
    0.372340 Defs.GAllTrumps
    ---------
    Defs.South Defs.East Defs.North Defs.West
    |}];

  print_endline @@ show game;
  (* TODO: how to remove `Game.game` and just have `game` ? *)
  [%expect
    {|
    State: Game.SDealRest
    Game: Defs.CNo Defs.GAllTrumps Defs.East
    ---
    Bid History:
    Defs.East { Bid.game = Defs.GClubs; bidder = Defs.East; counter = Defs.CNo }
    Defs.North { Bid.game = Defs.GHearts; bidder = Defs.North; counter = Defs.CNo }
    Defs.West pass
    Defs.South pass
    Defs.East { Bid.game = Defs.GAllTrumps; bidder = Defs.East; counter = Defs.CNo }
    Defs.North pass
    Defs.West pass
    Defs.South pass
    ---
    Players:
    Player:
    Name:Defs.South
    Type:Defs.Machine
    Pos:Defs.South
    Partner:Defs.NorthPoints:0
    Cards:
    { Card.suite = Defs.SSpades; value = Defs.Eight }
    { Card.suite = Defs.SSpades; value = Defs.Ten }
    { Card.suite = Defs.SSpades; value = Defs.Ace }
    { Card.suite = Defs.SHearts; value = Defs.Eight }
    { Card.suite = Defs.SSpades; value = Defs.Nine }
    Combinations:
    Player:
    Name:Defs.East
    Type:Defs.Machine
    Pos:Defs.East
    Partner:Defs.WestPoints:0
    Cards:
    { Card.suite = Defs.SSpades; value = Defs.Jack }
    { Card.suite = Defs.SClubs; value = Defs.Jack }
    { Card.suite = Defs.SDiamonds; value = Defs.King }
    { Card.suite = Defs.SDiamonds; value = Defs.Ace }
    { Card.suite = Defs.SClubs; value = Defs.Nine }
    Combinations:
    Player:
    Name:Defs.North
    Type:Defs.Machine
    Pos:Defs.North
    Partner:Defs.SouthPoints:0
    Cards:
    { Card.suite = Defs.SHearts; value = Defs.Ten }
    { Card.suite = Defs.SHearts; value = Defs.Ace }
    { Card.suite = Defs.SHearts; value = Defs.Jack }
    { Card.suite = Defs.SDiamonds; value = Defs.Ten }
    { Card.suite = Defs.SClubs; value = Defs.Eight }
    Combinations:
    Player:
    Name:Defs.West
    Type:Defs.Machine
    Pos:Defs.West
    Partner:Defs.EastPoints:0
    Cards:
    { Card.suite = Defs.SDiamonds; value = Defs.Eight }
    { Card.suite = Defs.SSpades; value = Defs.Seven }
    { Card.suite = Defs.SDiamonds; value = Defs.Queen }
    { Card.suite = Defs.SHearts; value = Defs.Seven }
    { Card.suite = Defs.SClubs; value = Defs.King }
    Combinations:
    |}]

let%expect_test "do_bidding_2" =
  let test_deck =
    [
      (* south *)
      Card.make SHearts Ace;
      Card.make SDiamonds Seven;
      Card.make SSpades Eight;
      Card.make SHearts Seven;
      Card.make SClubs Eight;
      (* east *)
      Card.make SDiamonds King;
      Card.make SDiamonds Ten;
      Card.make SClubs Ten;
      Card.make SDiamonds Queen;
      Card.make SDiamonds Nine;
      (* north *)
      Card.make SDiamonds Ace;
      Card.make SClubs Seven;
      Card.make SClubs Ace;
      Card.make SHearts Ten;
      Card.make SDiamonds Eight;
      (* west *)
      Card.make SDiamonds Jack;
      Card.make SSpades Ten;
      Card.make SHearts Ace;
      Card.make SHearts Nine;
      Card.make SSpades Nine;
    ]
  in
  let game = test_setup_do_bidding test_deck in

  print_endline @@ show game;
  (* TODO: how to remove `Game.game` and just have `game` ? *)
  [%expect
    {|
    Defs.East
    0.611940 Defs.GDiamonds
    0.500000 Defs.GNoTrumps
    0.436170 Defs.GAllTrumps
    ---------
    Defs.North
    0.592593 Defs.GNoTrumps
    0.477612 Defs.GSpades
    0.477612 Defs.GDiamonds
    ---------
    Defs.West
    0.734043 Defs.GAllTrumps
    0.611940 Defs.GDiamonds
    0.552239 Defs.GSpades
    ---------
    Defs.South
    0.203704 Defs.GNoTrumps
    0.164179 Defs.GSpades
    0.164179 Defs.GDiamonds
    ---------
    Defs.East
    0.611940 Defs.GDiamonds
    0.500000 Defs.GNoTrumps
    0.436170 Defs.GAllTrumps
    ---------
    Defs.North
    0.592593 Defs.GNoTrumps
    0.477612 Defs.GSpades
    0.477612 Defs.GDiamonds
    ---------
    State: Game.SDealRest
    Game: Defs.CNo Defs.GAllTrumps Defs.West
    ---
    Bid History:
    Defs.East { Bid.game = Defs.GDiamonds; bidder = Defs.East; counter = Defs.CNo }
    Defs.North pass
    Defs.West { Bid.game = Defs.GAllTrumps; bidder = Defs.West; counter = Defs.CNo }
    Defs.South pass
    Defs.East pass
    Defs.North pass
    ---
    Players:
    Player:
    Name:Defs.South
    Type:Defs.Machine
    Pos:Defs.South
    Partner:Defs.NorthPoints:0
    Cards:
    { Card.suite = Defs.SHearts; value = Defs.Ace }
    { Card.suite = Defs.SDiamonds; value = Defs.Seven }
    { Card.suite = Defs.SSpades; value = Defs.Eight }
    { Card.suite = Defs.SHearts; value = Defs.Seven }
    { Card.suite = Defs.SClubs; value = Defs.Eight }
    Combinations:
    Player:
    Name:Defs.East
    Type:Defs.Machine
    Pos:Defs.East
    Partner:Defs.WestPoints:0
    Cards:
    { Card.suite = Defs.SDiamonds; value = Defs.King }
    { Card.suite = Defs.SDiamonds; value = Defs.Ten }
    { Card.suite = Defs.SClubs; value = Defs.Ten }
    { Card.suite = Defs.SDiamonds; value = Defs.Queen }
    { Card.suite = Defs.SDiamonds; value = Defs.Nine }
    Combinations:
    Player:
    Name:Defs.North
    Type:Defs.Machine
    Pos:Defs.North
    Partner:Defs.SouthPoints:0
    Cards:
    { Card.suite = Defs.SDiamonds; value = Defs.Ace }
    { Card.suite = Defs.SClubs; value = Defs.Seven }
    { Card.suite = Defs.SClubs; value = Defs.Ace }
    { Card.suite = Defs.SHearts; value = Defs.Ten }
    { Card.suite = Defs.SDiamonds; value = Defs.Eight }
    Combinations:
    Player:
    Name:Defs.West
    Type:Defs.Machine
    Pos:Defs.West
    Partner:Defs.EastPoints:0
    Cards:
    { Card.suite = Defs.SDiamonds; value = Defs.Jack }
    { Card.suite = Defs.SSpades; value = Defs.Ten }
    { Card.suite = Defs.SHearts; value = Defs.Ace }
    { Card.suite = Defs.SHearts; value = Defs.Nine }
    { Card.suite = Defs.SSpades; value = Defs.Nine }
    Combinations:
    |}]

let%expect_test "do_bidding_3" =
  let test_deck =
    [
      (* south *)
      Card.make SClubs Jack;
      Card.make SSpades Nine;
      Card.make SSpades Jack;
      Card.make SHearts Ace;
      Card.make SHearts Ten;
      (* east *)
      Card.make SDiamonds King;
      Card.make SSpades Ace;
      Card.make SClubs Ten;
      Card.make SClubs Queen;
      Card.make SClubs Ace;
      (* north *)
      Card.make SHearts Nine;
      Card.make SSpades Queen;
      Card.make SClubs Eight;
      Card.make SDiamonds Queen;
      Card.make SSpades Eight;
      (* west *)
      Card.make SDiamonds Jack;
      Card.make SClubs Seven;
      Card.make SHearts Queen;
      Card.make SDiamonds Ten;
      Card.make SHearts Eight;
    ]
  in
  let game = test_setup_do_bidding test_deck in

  print_endline @@ show game;
  (* TODO: how to remove `Game.game` and just have `game` ? *)
  [%expect
    {|
    Defs.East
    0.722222 Defs.GNoTrumps
    0.582090 Defs.GSpades
    0.582090 Defs.GDiamonds
    ---------
    Defs.North
    0.298507 Defs.GHearts
    0.212766 Defs.GAllTrumps
    0.111111 Defs.GNoTrumps
    ---------
    Defs.West
    0.492537 Defs.GDiamonds
    0.351064 Defs.GAllTrumps
    0.277778 Defs.GNoTrumps
    ---------
    Defs.South
    0.850746 Defs.GSpades
    0.797872 Defs.GAllTrumps
    0.641791 Defs.GClubs
    ---------
    Defs.East
    0.722222 Defs.GNoTrumps
    0.582090 Defs.GSpades
    0.582090 Defs.GDiamonds
    ---------
    Defs.North
    0.298507 Defs.GHearts
    0.212766 Defs.GAllTrumps
    0.111111 Defs.GNoTrumps
    ---------
    Defs.West
    0.492537 Defs.GDiamonds
    0.351064 Defs.GAllTrumps
    0.277778 Defs.GNoTrumps
    ---------
    State: Game.SDealRest
    Game: Defs.CNo Defs.GAllTrumps Defs.South
    ---
    Bid History:
    Defs.East { Bid.game = Defs.GNoTrumps; bidder = Defs.East; counter = Defs.CNo }
    Defs.North pass
    Defs.West pass
    Defs.South { Bid.game = Defs.GAllTrumps; bidder = Defs.South; counter = Defs.CNo }
    Defs.East pass
    Defs.North pass
    Defs.West pass
    ---
    Players:
    Player:
    Name:Defs.South
    Type:Defs.Machine
    Pos:Defs.South
    Partner:Defs.NorthPoints:0
    Cards:
    { Card.suite = Defs.SClubs; value = Defs.Jack }
    { Card.suite = Defs.SSpades; value = Defs.Nine }
    { Card.suite = Defs.SSpades; value = Defs.Jack }
    { Card.suite = Defs.SHearts; value = Defs.Ace }
    { Card.suite = Defs.SHearts; value = Defs.Ten }
    Combinations:
    Player:
    Name:Defs.East
    Type:Defs.Machine
    Pos:Defs.East
    Partner:Defs.WestPoints:0
    Cards:
    { Card.suite = Defs.SDiamonds; value = Defs.King }
    { Card.suite = Defs.SSpades; value = Defs.Ace }
    { Card.suite = Defs.SClubs; value = Defs.Ten }
    { Card.suite = Defs.SClubs; value = Defs.Queen }
    { Card.suite = Defs.SClubs; value = Defs.Ace }
    Combinations:
    Player:
    Name:Defs.North
    Type:Defs.Machine
    Pos:Defs.North
    Partner:Defs.SouthPoints:0
    Cards:
    { Card.suite = Defs.SHearts; value = Defs.Nine }
    { Card.suite = Defs.SSpades; value = Defs.Queen }
    { Card.suite = Defs.SClubs; value = Defs.Eight }
    { Card.suite = Defs.SDiamonds; value = Defs.Queen }
    { Card.suite = Defs.SSpades; value = Defs.Eight }
    Combinations:
    Player:
    Name:Defs.West
    Type:Defs.Machine
    Pos:Defs.West
    Partner:Defs.EastPoints:0
    Cards:
    { Card.suite = Defs.SDiamonds; value = Defs.Jack }
    { Card.suite = Defs.SClubs; value = Defs.Seven }
    { Card.suite = Defs.SHearts; value = Defs.Queen }
    { Card.suite = Defs.SDiamonds; value = Defs.Ten }
    { Card.suite = Defs.SHearts; value = Defs.Eight }
    Combinations:
    |}]

let%expect_test "do_bidding_4" =
  let test_deck =
    [
      (* south *)
      Card.make SClubs Jack;
      Card.make SHearts Eight;
      Card.make SHearts Queen;
      Card.make SHearts Ace;
      Card.make SHearts Ten;
      (* east *)
      Card.make SClubs Nine;
      Card.make SClubs Jack;
      Card.make SClubs Ten;
      Card.make SClubs King;
      Card.make SClubs Ace;
      (* north *)
      Card.make SHearts Nine;
      Card.make SSpades Queen;
      Card.make SDiamonds Eight;
      Card.make SDiamonds Queen;
      Card.make SSpades Eight;
      (* west *)
      Card.make SDiamonds Jack;
      Card.make SDiamonds Nine;
      Card.make SDiamonds Ace;
      Card.make SDiamonds Ten;
      Card.make SDiamonds King;
    ]
  in
  let game = test_setup_do_bidding test_deck in

  print_endline @@ show game;
  (* TODO: how to remove `Game.game` and just have `game` ? *)
  [%expect
    {|
    Defs.East
    0.880597 Defs.GClubs
    0.627660 Defs.GAllTrumps
    0.500000 Defs.GNoTrumps
    ---------
    Defs.North
    0.298507 Defs.GHearts
    0.212766 Defs.GAllTrumps
    0.111111 Defs.GNoTrumps
    ---------
    Defs.West
    0.880597 Defs.GDiamonds
    0.627660 Defs.GAllTrumps
    0.500000 Defs.GNoTrumps
    ---------
    Defs.South
    0.656716 Defs.GClubs
    0.481481 Defs.GNoTrumps
    0.468085 Defs.GAllTrumps
    ---------
    Defs.East
    0.880597 Defs.GClubs
    0.627660 Defs.GAllTrumps
    0.500000 Defs.GNoTrumps
    ---------
    Defs.North
    0.298507 Defs.GHearts
    0.212766 Defs.GAllTrumps
    0.111111 Defs.GNoTrumps
    ---------
    Defs.West
    0.880597 Defs.GDiamonds
    0.627660 Defs.GAllTrumps
    0.500000 Defs.GNoTrumps
    ---------
    Defs.South
    0.656716 Defs.GClubs
    0.481481 Defs.GNoTrumps
    0.468085 Defs.GAllTrumps
    ---------
    State: Game.SDealRest
    Game: Defs.CNo Defs.GAllTrumps Defs.East
    ---
    Bid History:
    Defs.East { Bid.game = Defs.GClubs; bidder = Defs.East; counter = Defs.CNo }
    Defs.North pass
    Defs.West { Bid.game = Defs.GDiamonds; bidder = Defs.West; counter = Defs.CNo }
    Defs.South pass
    Defs.East { Bid.game = Defs.GAllTrumps; bidder = Defs.East; counter = Defs.CNo }
    Defs.North pass
    Defs.West pass
    Defs.South pass
    ---
    Players:
    Player:
    Name:Defs.South
    Type:Defs.Machine
    Pos:Defs.South
    Partner:Defs.NorthPoints:0
    Cards:
    { Card.suite = Defs.SClubs; value = Defs.Jack }
    { Card.suite = Defs.SHearts; value = Defs.Eight }
    { Card.suite = Defs.SHearts; value = Defs.Queen }
    { Card.suite = Defs.SHearts; value = Defs.Ace }
    { Card.suite = Defs.SHearts; value = Defs.Ten }
    Combinations:
    Player:
    Name:Defs.East
    Type:Defs.Machine
    Pos:Defs.East
    Partner:Defs.WestPoints:0
    Cards:
    { Card.suite = Defs.SClubs; value = Defs.Nine }
    { Card.suite = Defs.SClubs; value = Defs.Jack }
    { Card.suite = Defs.SClubs; value = Defs.Ten }
    { Card.suite = Defs.SClubs; value = Defs.King }
    { Card.suite = Defs.SClubs; value = Defs.Ace }
    Combinations:
    Player:
    Name:Defs.North
    Type:Defs.Machine
    Pos:Defs.North
    Partner:Defs.SouthPoints:0
    Cards:
    { Card.suite = Defs.SHearts; value = Defs.Nine }
    { Card.suite = Defs.SSpades; value = Defs.Queen }
    { Card.suite = Defs.SDiamonds; value = Defs.Eight }
    { Card.suite = Defs.SDiamonds; value = Defs.Queen }
    { Card.suite = Defs.SSpades; value = Defs.Eight }
    Combinations:
    Player:
    Name:Defs.West
    Type:Defs.Machine
    Pos:Defs.West
    Partner:Defs.EastPoints:0
    Cards:
    { Card.suite = Defs.SDiamonds; value = Defs.Jack }
    { Card.suite = Defs.SDiamonds; value = Defs.Nine }
    { Card.suite = Defs.SDiamonds; value = Defs.Ace }
    { Card.suite = Defs.SDiamonds; value = Defs.Ten }
    { Card.suite = Defs.SDiamonds; value = Defs.King }
    Combinations:
    |}]

let%expect_test "announce_combination" =
  let player = Player.make South in
  let cards =
    [
      Card.make SClubs Queen;
      Card.make SSpades Queen;
      Card.make SDiamonds Queen;
      Card.make SHearts Queen;
      Card.make SHearts King;
      Card.make SHearts Ace;
      Card.make SHearts Jack;
      Card.make SDiamonds Ace;
    ]
  in
  let player = Player.store_cards player cards in

  let player, _ = announce_combination player in
  printf "%s" @@ Player.show player;
  [%expect
    {|
    Player:
    Name:Defs.South
    Type:Defs.Machine
    Pos:Defs.South
    Partner:Defs.NorthPoints:0
    Cards:
    { Card.suite = Defs.SClubs; value = Defs.Queen }
    { Card.suite = Defs.SSpades; value = Defs.Queen }
    { Card.suite = Defs.SDiamonds; value = Defs.Queen }
    { Card.suite = Defs.SHearts; value = Defs.Queen }
    { Card.suite = Defs.SHearts; value = Defs.King }
    { Card.suite = Defs.SHearts; value = Defs.Ace }
    { Card.suite = Defs.SHearts; value = Defs.Jack }
    { Card.suite = Defs.SDiamonds; value = Defs.Ace }
    Combinations:
    (Defs.Carre Defs.Queen)
    |}]
