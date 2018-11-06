open Lambda_let_paren

exception RuntimeError of string

let runtime_error s = raise (RuntimeError s)

let rec eval (env : env) (idxenv : idxenv) (exp : exp) = VInt 1

(* entrypoint *)
let start exp = eval Environment.empty Environment.empty exp
