open! Core
open Defs
open! Poly

(* TODO: read those from a config file *)
let raiseNoBidScore = 0.50
let raiseBidScore = 0.70
let counterBidScore = 0.75
let raisePartnerNoTrumpsScore = 0.85

type bid = { game : cgame; bidder : player_pos; counter : ccounter }
[@@deriving show]

let make_bid chosen_game bidder counter =
  { game = chosen_game; bidder; counter }

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
  sprintf "State: %s\nGame: %s %s %s\nBid History:\n%s"
    (show_game_state game.state)
    counter_s chosen_game_s bidder_s
    (List.fold bid_history ~init:"" ~f:(fun acc el -> sprintf "%s%s" acc el))

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
  let best_score, best_game = List.nth_exn sorted_scores 0 in
  (best_score, best_game)

let best_bid cards current_bid player_pos =
  let best_score, best_game = calculate_best_game cards in
  (* TODO: if you bid color and then your partner bids another color -> bid all trumps *)
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
      [%expect {| Defs.GAllTrumps |}];
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

  let bid = best_bid cards None South in
  match bid with
  | None -> [%expect.unreachable]
  | Some b ->
      printf "%s" @@ show_cgame b.game;
      [%expect {| Defs.GAllTrumps |}];
      printf "%s" @@ show_ccounter b.counter;
      [%expect {| Defs.CNo |}]

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

  let bid = best_bid cards None South in
  match bid with
  | None -> [%expect.unreachable]
  | Some b ->
      printf "%s" @@ show_cgame b.game;
      [%expect {| Defs.GAllTrumps |}];
      printf "%s" @@ show_ccounter b.counter;
      [%expect {| Defs.CNo |}]

let%expect_test "test_best_bid_no_trumps" =
  let cards =
    [
      Card.make SDiamonds King;
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
      [%expect {| Defs.GNoTrumps |}];
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
      [%expect {| Defs.GClubs |}];
      printf "%s" @@ show_ccounter b.counter;
      [%expect {| Defs.CNo |}]

let%expect_test "do_bidding" =
  let test_deck =
    [
      (* dealer cards *)
      Card.make SSpades Jack;
      Card.make SSpades Jack;
      Card.make SSpades Jack;
      Card.make SSpades Jack;
      Card.make SSpades Jack;
      (* starting player *)
      Card.make SSpades Jack;
      Card.make SSpades Jack;
      Card.make SSpades Jack;
      Card.make SSpades Jack;
      Card.make SSpades Jack;
      (* player 3 (dealer partner) *)
      Card.make SSpades Jack;
      Card.make SSpades Jack;
      Card.make SSpades Jack;
      Card.make SSpades Jack;
      Card.make SSpades Jack;
      (* player 4 (starting player partner) *)
      Card.make SSpades Jack;
      Card.make SSpades Jack;
      Card.make SSpades Jack;
      Card.make SSpades Jack;
      Card.make SSpades Jack;
    ]
  in
  let game = make |> do_shuffle in
  let players, deck = deal_cards game.players 5 (Deck.of_cards test_deck) [] in
  let game = { game with state = SBidding; players; deck } in
  let game = do_bidding game in
  print_endline @@ show game;
  [%expect
    {|
    State: Game.SDealRest
    Game: Defs.CReCounter Defs.GSpades Defs.West
    Bid History:
    Defs.East { Game.game = Defs.GSpades; bidder = Defs.East; counter = Defs.CNo }
    Defs.North { Game.game = Defs.GSpades; bidder = Defs.North; counter = Defs.CCounter }
    Defs.West { Game.game = Defs.GSpades; bidder = Defs.West; counter = Defs.CReCounter }
    Defs.South pass
    Defs.East pass
    Defs.North pass
    |}]
