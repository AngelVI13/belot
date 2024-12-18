type t

val make : t
val shuffle : t -> t
val print : t -> unit
val deal : t -> int -> t * t
val to_cards: t -> Card.t list
val of_cards: Card.t list -> t
