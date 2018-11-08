open Lambda_let_paren

exception RuntimeError of string

let runtime_error s = raise (RuntimeError s)

let eval_idx idxenv = function
  | IVar i -> Environment.lookup i idxenv
  | INat i -> INat i

let rec eval (env : env) (idxenv : idxenv) = function
  | EVar x -> Environment.lookup x env
  | EInt i -> VInt i
  | EBool b -> VBool b
  | EAbs (x, e) -> VProc (x, e, env)
  | EApp (e1, e2) ->
      let v1 = eval env idxenv e1 in
      let v2 = eval env idxenv e2 in
      begin match v1 with
        | VProc (x, e, env') ->
            let env'' = Environment.extend x v2 env' in
            eval env'' idxenv e
        | _ -> runtime_error "not a function"
      end
  | ELet (x, e1, e2) ->
      let v1 = eval env idxenv e1 in
      let env' = Environment.extend x v1 env in
      eval env' idxenv e2
  | EArray es ->
      let vs = Array.of_list @@ List.map (eval env idxenv) es in
      VArray vs
  | EArrayGet (e, i) ->
      let v = eval env idxenv e in
      let INat idx = eval_idx idxenv i in
      let idx' = idx - 1 in
      begin match v with
        | VArray arr -> Array.get arr idx'
        | _ -> runtime_error "not an array"
      end
  | EArrayModify (e1, i, e2) ->
      let v1 = eval env idxenv e1 in
      let INat idx = eval_idx idxenv i in
      let idx' = idx - 1 in
      let v2 = eval env idxenv e2 in
      begin match v1 with
        | VArray arr ->
            let arr' = Array.copy arr in
            arr'.(idx') <- v2;
            VArray arr'
        | _ -> runtime_error "not an array"
      end
  | EIdxAbs (iv, e) -> VIdxAbs (iv, e)
  | EIdxApp (e, i) ->
      let v = eval env idxenv e in
      let INat idx = eval_idx idxenv i in
      begin match v with
        | VIdxAbs (iv, e) ->
            let idxenv' = Environment.extend iv i idxenv in
            eval env idxenv' e
        | _ -> runtime_error "not a index function"
      end

(* entrypoint *)
let start exp = eval Environment.empty Environment.empty exp
