type id = string

type tyvar = int

type label = string

type 'a record = (label * 'a) list

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
