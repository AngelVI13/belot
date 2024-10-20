open Defs

type t

val make: csuite -> cvalue -> t
val show: t -> Ppx_deriving_runtime.string 
