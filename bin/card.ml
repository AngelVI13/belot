open Defs

type t = { suite : csuite; value : cvalue } [@@deriving show]

let make suite value = { suite; value }
let show = show
