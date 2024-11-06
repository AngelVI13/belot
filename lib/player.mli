open Defs
type t

val make: player_pos -> t
val show: t -> string
val store_cards: t -> Card.t list -> t
val store_combos: t -> ccombination list -> t
val cards: t -> Card.t list
val new_round: t -> t
val pos: t -> player_pos
val play_card: t -> t * Card.t
