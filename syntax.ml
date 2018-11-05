type id = string

type binop =
  | Plus
  | Mult
  | Lt

type exp =
  | Var of id
  | Int of int
  | Bool of bool
  | BinOp of binop * exp * exp
  | IfThenElse of exp * exp * exp
  | Fun of id * exp
  | App of exp * exp
  | Let of id * exp * exp
  | Record of (id * exp) list
  | RecordGet of id * exp
  | RecordModify of exp * id * exp
