type id = string

type exp =
  | Var of id
  | Int of int
  | Let of id * exp * exp
