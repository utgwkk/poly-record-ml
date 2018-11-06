open Lambda_let_paren

exception RuntimeError of string

let runtime_error s = raise (RuntimeError s)

let rec eval (env : env) (exp : exp) = VInt 1
