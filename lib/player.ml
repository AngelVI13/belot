open Core
open Defs
open! Poly
open! Bid
open! Cards

type t = {
  name : string;
  cards : Card.t list;
  points : int;
  partner : player_pos;
  pos : player_pos;
  type_ : player_type;
  combinations : ccombination list;
}

let show player =
  let type_s = show_player_type player.type_ in
  let pos_s = show_player_pos player.pos in
  let partner_s = show_player_pos player.partner in
  let cards_s = List.map player.cards ~f:Card.show in
  let combinations_s = List.map player.combinations ~f:show_ccombination in
  sprintf
    "Player:\n\
     Name:%s\n\
     Type:%s\n\
     Pos:%s\n\
     Partner:%s\n\
     Points:%d\n\
     Cards:%s\n\
     Combinations:%s\n"
    player.name type_s pos_s partner_s player.points
    (List.fold cards_s ~init:"" ~f:(fun acc el -> sprintf "%s\n%s" acc el))
    (List.fold combinations_s ~init:"" ~f:(fun acc el ->
         sprintf "%s\n%s" acc el))

let make player_pos =
  {
    name = show_player_pos player_pos;
    cards = [];
    points = 0;
    partner = partner player_pos;
    pos = player_pos;
    type_ = Machine;
    combinations = [];
  }

let store_cards player cards = { player with cards = cards @ player.cards }
let store_combos player combinations = { player with combinations }
let new_round player = { player with cards = []; combinations = [] }
let cards player = player.cards
let pos player = player.pos

let remove_card_by_idx player idx =
  let card = List.nth_exn player.cards idx in
  let cards = List.filteri player.cards ~f:(fun i _ -> i <> idx) in
  ({ player with cards }, card)

let play_card player chosen_game current_hand =
  match current_hand with
  (* if nothing has been played -> play the first card you have *)
  | [] -> remove_card_by_idx player 0
  | hand_card :: hand_cards -> (
      let _ = hand_cards in
      match chosen_game.game with
      | GClubs | GDiamonds | GHearts | GSpades -> remove_card_by_idx player 0
      | GNoTrumps -> (
          let target_suite = Card.suite hand_card in
          let card_same_suite =
            List.findi player.cards ~f:(fun _ card ->
                Card.suite card = target_suite)
          in
          match card_same_suite with
          | Some (idx, _) -> remove_card_by_idx player idx
          | None -> remove_card_by_idx player 0)
      | GAllTrumps ->
          let target_suite = Card.suite hand_card in
          let current_hand_by_power =
            descending_by_suite current_hand target_suite
          in
          let highest_value = List.nth_exn current_hand_by_power 0 in
          let my_cards_bigger_by_suite =
            bigger_by_suite player.cards target_suite (Card.value highest_value)
          in
          (* TODO: finish this. check if we have higher value of target suite
             than the highest_value
             if yes -> return it
             if no -> return first random card *)
          let _ = my_cards_bigger_by_suite in
          remove_card_by_idx player 0)
(* TODO: finish this: If you have a card of the same suite as hand_card -> play it
   else if it is a color game -> play a trump card. If you dont have a trump
   card or its not a color game -> play anything else *)
