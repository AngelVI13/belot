open Defs

type t

val make: csuite -> cvalue -> t
val show: t -> Ppx_deriving_runtime.string 
val is_trump: t -> csuite -> bool
val value: t -> cvalue
val suite: t -> csuite
val compare_by_value: t -> t -> int
