open Core
open Defs

type t = {
  name : string;
  cards : Card.t list;
  announce : cgame option;
  points : int;
  partner : player_pos;
  pos : player_pos;
  type_ : player_type;
}

(*let show = "not implemented"*)

let make player_pos =
  {
    name = show_player_pos player_pos;
    cards = [];
    announce = None;
    points = 0;
    partner = partner player_pos;
    pos = player_pos;
    type_ = Machine;
  }

let store_cards player cards = { player with cards = cards @ player.cards }
let new_round player = { player with cards = []; announce = None }
let cards player = player.cards
