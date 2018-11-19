type id = string

type tyvar = int

type label = string

type binOp =
  | Plus
  | Mult
  | Lt
  | Assign

let string_of_binOp = function
  | Plus -> "Plus"
  | Mult -> "Mult"
  | Lt -> "Lt"
  | Assign -> "Assign"
