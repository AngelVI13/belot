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

let show player =
  let type_s = show_player_type player.type_ in
  let pos_s = show_player_pos player.pos in
  let partner_s = show_player_pos player.partner in
  let announce_s =
    match player.announce with None -> "NoAnnonce" | Some s -> show_cgame s
  in
  let cards_s = List.map player.cards ~f:Card.show in
  sprintf
    "Player:\n\
     Name:%s\n\
     Type:%s\n\
     Pos:%s\n\
     Partner:%sPoints:%d\n\
     Announce:%s\n\
     Cards:%s\n"
    player.name type_s pos_s partner_s player.points announce_s
    (List.fold cards_s ~init:"" ~f:(fun acc el -> sprintf "%s\n%s" acc el))

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
let pos player = player.pos
