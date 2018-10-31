type id = string

type exp =
  | Var of id
  | Int of int
  | Let of id * exp * exp
  | Record of (id * exp) list
  | RecordGet of id * exp
