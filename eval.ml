open Syntax

type value =
  | VInt of int

let string_of_value = function
  | VInt i -> string_of_int i

let rec eval env = function
  | Var x -> Env.lookup x env
  | Int i -> VInt i
  | Let (x, e1, e2) ->
      let v1 = eval env e1 in
      let env' = Env.extend x v1 env in
      eval env' e2
