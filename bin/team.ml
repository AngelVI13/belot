open Defs

type t = { players : player_pos list; points : int } [@@deriving show]

let make (player1, player2) = { players = [ player1; player2 ]; points = 0 }
