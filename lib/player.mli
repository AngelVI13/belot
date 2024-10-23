open Defs
type t

val make: player_pos -> t
(*val show: t -> Ppx_deriving_runtime.string *)
val store_cards: t -> Card.t list -> t
val new_round: t -> t
