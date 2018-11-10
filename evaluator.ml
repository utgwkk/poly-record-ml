open Implementation
open Syntax

exception RuntimeError of string

let runtime_error s = raise (RuntimeError s)

let rec eval_idx idxenv = function
  | IVar i -> eval_idx idxenv (Environment.lookup i idxenv)
  | INat i -> i

let rec calc_binop op lhs rhs = match (op, lhs, rhs) with
  | Plus, VInt i1, VInt i2 -> VInt (i1 + i2)
  | Mult, VInt i1, VInt i2 -> VInt (i1 * i2)
  | Lt, VInt i1, VInt i2 -> VBool (i1 < i2)
  | _ -> runtime_error "both operands must be integer"

let rec eval (env : env) (idxenv : idxenv) = function
  | EVar x -> Environment.lookup x env
  | EInt i -> VInt i
  | EBool b -> VBool b
  | EBinOp (op, e1, e2) ->
      let v1 = eval env idxenv e1 in
      let v2 = eval env idxenv e2 in
      calc_binop op v1 v2
  | EIfThenElse (e1, e2, e3) ->
      let v1 = eval env idxenv e1 in
      begin match v1 with
        | VBool b ->
            if b then eval env idxenv e2
            else eval env idxenv e3
        | _ -> runtime_error "condition must be boolean"
      end
  | EAbs (x, e) -> VProc (x, e, env, idxenv)
  | EApp (e1, e2) ->
      let v1 = eval env idxenv e1 in
      let v2 = eval env idxenv e2 in
      begin match v1 with
        | VProc (x, e, env', idxenv') ->
            let env'' = Environment.extend x v2 env' in
            eval env'' idxenv' e
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
      let idx = eval_idx idxenv i in
      let idx' = idx - 1 in
      begin match v with
        | VArray arr -> Array.get arr idx'
        | _ -> runtime_error "not an array"
      end
  | EArrayModify (e1, i, e2) ->
      let v1 = eval env idxenv e1 in
      let idx = eval_idx idxenv i in
      let idx' = idx - 1 in
      let v2 = eval env idxenv e2 in
      begin match v1 with
        | VArray arr ->
            let arr' = Array.copy arr in
            arr'.(idx') <- v2;
            VArray arr'
        | _ -> runtime_error "not an array"
      end
  | EIdxAbs (iv, e) -> VIdxAbs (iv, e, env, idxenv)
  | EIdxApp (e, i) ->
      let v = eval env idxenv e in
      begin match v with
        | VIdxAbs (iv, e, env', idxenv') ->
            let idxenv'' = Environment.extend iv i idxenv' in
            eval env' idxenv'' e
        | _ -> runtime_error "not a index function"
      end

(* entrypoint *)
let start exp = eval Environment.empty Environment.empty exp
