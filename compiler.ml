module Lld = Lambda_let_dot
module Llp = Lambda_let_paren

let rec compile (lbenv : Lld.lbenv) (tyenv : Lld.tyenv) (exp : Lld.exp) = Llp.EInt 1

(* entrypoint *)
let start exp = compile Environment.empty Environment.empty exp
