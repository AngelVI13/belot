open! Core
open Defs
open Cards
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

let find_carre_combinations cards =
  let sorted =
    List.sort_and_group cards ~compare:(fun c1 c2 ->
        let c1_int = cvalue_to_enum @@ Card.value c1 in
        let c2_int = cvalue_to_enum @@ Card.value c2 in
        if c1_int < c2_int then -1 else if c1_int > c2_int then 1 else 0)
  in
  List.filter sorted ~f:(fun same_list -> List.length same_list = 4)

let find_consecutive_combinations cards =
  let grouped =
    List.group cards ~break:(fun c1 c2 -> Card.suite c1 = Card.suite c2)
  in
  let sorted =
    List.map grouped ~f:(fun c ->
        List.sort c ~compare:(fun c1 c2 ->
            let c1_int = cvalue_to_enum @@ Card.value c1 in
            let c2_int = cvalue_to_enum @@ Card.value c2 in
            if c1_int < c2_int then -1 else if c1_int > c2_int then 1 else 0))
  in
  let _ = sorted in
  (* TODO: finish this *)
  []

let announce_combination player =
  let cards = Player.cards player in
  let _ = cards in
  []

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
    Defs.East { Game.game = Defs.GClubs; bidder = Defs.East; counter = Defs.CNo }
    Defs.North { Game.game = Defs.GHearts; bidder = Defs.North; counter = Defs.CNo }
    Defs.West pass
    Defs.South pass
    Defs.East { Game.game = Defs.GAllTrumps; bidder = Defs.East; counter = Defs.CNo }
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
    Announce:NoAnnonce
    Cards:
    { Card.suite = Defs.SSpades; value = Defs.Eight }
    { Card.suite = Defs.SSpades; value = Defs.Ten }
    { Card.suite = Defs.SSpades; value = Defs.Ace }
    { Card.suite = Defs.SHearts; value = Defs.Eight }
    { Card.suite = Defs.SSpades; value = Defs.Nine }
    Player:
    Name:Defs.East
    Type:Defs.Machine
    Pos:Defs.East
    Partner:Defs.WestPoints:0
    Announce:NoAnnonce
    Cards:
    { Card.suite = Defs.SSpades; value = Defs.Jack }
    { Card.suite = Defs.SClubs; value = Defs.Jack }
    { Card.suite = Defs.SDiamonds; value = Defs.King }
    { Card.suite = Defs.SDiamonds; value = Defs.Ace }
    { Card.suite = Defs.SClubs; value = Defs.Nine }
    Player:
    Name:Defs.North
    Type:Defs.Machine
    Pos:Defs.North
    Partner:Defs.SouthPoints:0
    Announce:NoAnnonce
    Cards:
    { Card.suite = Defs.SHearts; value = Defs.Ten }
    { Card.suite = Defs.SHearts; value = Defs.Ace }
    { Card.suite = Defs.SHearts; value = Defs.Jack }
    { Card.suite = Defs.SDiamonds; value = Defs.Ten }
    { Card.suite = Defs.SClubs; value = Defs.Eight }
    Player:
    Name:Defs.West
    Type:Defs.Machine
    Pos:Defs.West
    Partner:Defs.EastPoints:0
    Announce:NoAnnonce
    Cards:
    { Card.suite = Defs.SDiamonds; value = Defs.Eight }
    { Card.suite = Defs.SSpades; value = Defs.Seven }
    { Card.suite = Defs.SDiamonds; value = Defs.Queen }
    { Card.suite = Defs.SHearts; value = Defs.Seven }
    { Card.suite = Defs.SClubs; value = Defs.King }
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
    Defs.East { Game.game = Defs.GDiamonds; bidder = Defs.East; counter = Defs.CNo }
    Defs.North pass
    Defs.West { Game.game = Defs.GAllTrumps; bidder = Defs.West; counter = Defs.CNo }
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
    Announce:NoAnnonce
    Cards:
    { Card.suite = Defs.SHearts; value = Defs.Ace }
    { Card.suite = Defs.SDiamonds; value = Defs.Seven }
    { Card.suite = Defs.SSpades; value = Defs.Eight }
    { Card.suite = Defs.SHearts; value = Defs.Seven }
    { Card.suite = Defs.SClubs; value = Defs.Eight }
    Player:
    Name:Defs.East
    Type:Defs.Machine
    Pos:Defs.East
    Partner:Defs.WestPoints:0
    Announce:NoAnnonce
    Cards:
    { Card.suite = Defs.SDiamonds; value = Defs.King }
    { Card.suite = Defs.SDiamonds; value = Defs.Ten }
    { Card.suite = Defs.SClubs; value = Defs.Ten }
    { Card.suite = Defs.SDiamonds; value = Defs.Queen }
    { Card.suite = Defs.SDiamonds; value = Defs.Nine }
    Player:
    Name:Defs.North
    Type:Defs.Machine
    Pos:Defs.North
    Partner:Defs.SouthPoints:0
    Announce:NoAnnonce
    Cards:
    { Card.suite = Defs.SDiamonds; value = Defs.Ace }
    { Card.suite = Defs.SClubs; value = Defs.Seven }
    { Card.suite = Defs.SClubs; value = Defs.Ace }
    { Card.suite = Defs.SHearts; value = Defs.Ten }
    { Card.suite = Defs.SDiamonds; value = Defs.Eight }
    Player:
    Name:Defs.West
    Type:Defs.Machine
    Pos:Defs.West
    Partner:Defs.EastPoints:0
    Announce:NoAnnonce
    Cards:
    { Card.suite = Defs.SDiamonds; value = Defs.Jack }
    { Card.suite = Defs.SSpades; value = Defs.Ten }
    { Card.suite = Defs.SHearts; value = Defs.Ace }
    { Card.suite = Defs.SHearts; value = Defs.Nine }
    { Card.suite = Defs.SSpades; value = Defs.Nine }
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
    Defs.East { Game.game = Defs.GNoTrumps; bidder = Defs.East; counter = Defs.CNo }
    Defs.North pass
    Defs.West pass
    Defs.South { Game.game = Defs.GAllTrumps; bidder = Defs.South; counter = Defs.CNo }
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
    Announce:NoAnnonce
    Cards:
    { Card.suite = Defs.SClubs; value = Defs.Jack }
    { Card.suite = Defs.SSpades; value = Defs.Nine }
    { Card.suite = Defs.SSpades; value = Defs.Jack }
    { Card.suite = Defs.SHearts; value = Defs.Ace }
    { Card.suite = Defs.SHearts; value = Defs.Ten }
    Player:
    Name:Defs.East
    Type:Defs.Machine
    Pos:Defs.East
    Partner:Defs.WestPoints:0
    Announce:NoAnnonce
    Cards:
    { Card.suite = Defs.SDiamonds; value = Defs.King }
    { Card.suite = Defs.SSpades; value = Defs.Ace }
    { Card.suite = Defs.SClubs; value = Defs.Ten }
    { Card.suite = Defs.SClubs; value = Defs.Queen }
    { Card.suite = Defs.SClubs; value = Defs.Ace }
    Player:
    Name:Defs.North
    Type:Defs.Machine
    Pos:Defs.North
    Partner:Defs.SouthPoints:0
    Announce:NoAnnonce
    Cards:
    { Card.suite = Defs.SHearts; value = Defs.Nine }
    { Card.suite = Defs.SSpades; value = Defs.Queen }
    { Card.suite = Defs.SClubs; value = Defs.Eight }
    { Card.suite = Defs.SDiamonds; value = Defs.Queen }
    { Card.suite = Defs.SSpades; value = Defs.Eight }
    Player:
    Name:Defs.West
    Type:Defs.Machine
    Pos:Defs.West
    Partner:Defs.EastPoints:0
    Announce:NoAnnonce
    Cards:
    { Card.suite = Defs.SDiamonds; value = Defs.Jack }
    { Card.suite = Defs.SClubs; value = Defs.Seven }
    { Card.suite = Defs.SHearts; value = Defs.Queen }
    { Card.suite = Defs.SDiamonds; value = Defs.Ten }
    { Card.suite = Defs.SHearts; value = Defs.Eight }
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
    Defs.East { Game.game = Defs.GClubs; bidder = Defs.East; counter = Defs.CNo }
    Defs.North pass
    Defs.West { Game.game = Defs.GDiamonds; bidder = Defs.West; counter = Defs.CNo }
    Defs.South pass
    Defs.East { Game.game = Defs.GAllTrumps; bidder = Defs.East; counter = Defs.CNo }
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
    Announce:NoAnnonce
    Cards:
    { Card.suite = Defs.SClubs; value = Defs.Jack }
    { Card.suite = Defs.SHearts; value = Defs.Eight }
    { Card.suite = Defs.SHearts; value = Defs.Queen }
    { Card.suite = Defs.SHearts; value = Defs.Ace }
    { Card.suite = Defs.SHearts; value = Defs.Ten }
    Player:
    Name:Defs.East
    Type:Defs.Machine
    Pos:Defs.East
    Partner:Defs.WestPoints:0
    Announce:NoAnnonce
    Cards:
    { Card.suite = Defs.SClubs; value = Defs.Nine }
    { Card.suite = Defs.SClubs; value = Defs.Jack }
    { Card.suite = Defs.SClubs; value = Defs.Ten }
    { Card.suite = Defs.SClubs; value = Defs.King }
    { Card.suite = Defs.SClubs; value = Defs.Ace }
    Player:
    Name:Defs.North
    Type:Defs.Machine
    Pos:Defs.North
    Partner:Defs.SouthPoints:0
    Announce:NoAnnonce
    Cards:
    { Card.suite = Defs.SHearts; value = Defs.Nine }
    { Card.suite = Defs.SSpades; value = Defs.Queen }
    { Card.suite = Defs.SDiamonds; value = Defs.Eight }
    { Card.suite = Defs.SDiamonds; value = Defs.Queen }
    { Card.suite = Defs.SSpades; value = Defs.Eight }
    Player:
    Name:Defs.West
    Type:Defs.Machine
    Pos:Defs.West
    Partner:Defs.EastPoints:0
    Announce:NoAnnonce
    Cards:
    { Card.suite = Defs.SDiamonds; value = Defs.Jack }
    { Card.suite = Defs.SDiamonds; value = Defs.Nine }
    { Card.suite = Defs.SDiamonds; value = Defs.Ace }
    { Card.suite = Defs.SDiamonds; value = Defs.Ten }
    { Card.suite = Defs.SDiamonds; value = Defs.King }
    |}]

let%expect_test "announce_combination" =
  let player = Player.make South in
  let combinations = announce_combination player in
  printf "%s"
  @@ List.fold combinations ~init:"" ~f:(fun acc el ->
         sprintf "%s\n%s" acc (show_ccombination el));
  [%expect {||}]

let%expect_test "find_carre_combinations:no carre" =
  let cards =
    [
      Card.make SClubs Jack;
      Card.make SSpades Jack;
      Card.make SHearts Jack;
      Card.make SDiamonds Ace;
      Card.make SHearts Ten;
    ]
  in
  let combinations = find_carre_combinations cards in
  printf "%s"
  @@ List.fold combinations ~init:"" ~f:(fun acc el ->
         sprintf "%s\n%s" acc (show_card_list el));
  [%expect {| |}]

let%expect_test "find_carre_combinations:carre of jacks" =
  let cards =
    [
      Card.make SClubs Jack;
      Card.make SSpades Jack;
      Card.make SHearts Jack;
      Card.make SDiamonds Jack;
      Card.make SHearts Ten;
    ]
  in
  let combinations = find_carre_combinations cards in
  printf "%s"
  @@ List.fold combinations ~init:"" ~f:(fun acc el ->
         sprintf "%s\n%s" acc (show_card_list el));
  [%expect
    {|
    { Card.suite = Defs.SClubs; value = Defs.Jack }
    { Card.suite = Defs.SSpades; value = Defs.Jack }
    { Card.suite = Defs.SHearts; value = Defs.Jack }
    { Card.suite = Defs.SDiamonds; value = Defs.Jack }
    |}]

let%expect_test "find_consecutive_combinations:no combinations" =
  let cards =
    [
      Card.make SClubs Jack;
      Card.make SSpades Jack;
      Card.make SHearts Jack;
      Card.make SDiamonds Queen;
      Card.make SHearts Ten;
    ]
  in
  let combinations = find_consecutive_combinations cards in
  printf "%s"
  @@ List.fold combinations ~init:"" ~f:(fun acc el ->
         sprintf "%s\n%s" acc (show_card_list el));
  [%expect {| |}]
