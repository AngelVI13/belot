open Defs
type t

val make: player_pos -> t
val show: t -> Ppx_deriving_runtime.string 
