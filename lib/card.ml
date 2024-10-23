open Defs

type t = { suite : csuite; value : cvalue } [@@deriving show]

let make suite value = { suite; value }
let show = show
let is_trump card suite = card.suite = suite
let value card = card.value
