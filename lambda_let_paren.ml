open Syntax

type idxvar = int

type idx =
  | IVar of idxvar
  | INat of int

type ty =
  | TVar of tyvar
  | TInt
  | TBool
  | TFun of ty * ty
  | TRecord of (label * ty) list
  | TIdxFun of idxty list * ty
and idxty = label * ty
and kind =
  | KUniv
  | KRecord of (label * ty) list

type polyty = Forall of (tyvar * kind) list * ty

type subst = (tyvar * ty) list

let substitute subs t =
  let rec inner (tv, t) = function
    | TVar tv' -> if tv = tv' then t else TVar tv'
    | TFun (t1, t2) -> TFun (inner (tv, t) t1, inner (tv, t) t2)
    | TRecord xs ->
        let xs' = List.map (fun (l, t') -> (l, inner (tv, t') t)) xs in
        TRecord xs'
    | TIdxFun (xs, t') ->
        TIdxFun (xs, inner (tv, t) t')
    | t -> t
  in List.fold_right inner subs t

exception Label_not_found of label
exception Undefined_index_value
exception Not_a_record_type

let idx_value l =
  let rec inner n = function
    | [] -> raise (Label_not_found l)
    | (l', _) :: t ->
        if l = l' then n
        else inner (n + 1) t
  in function
  | TVar _ -> raise Undefined_index_value
  | TRecord xs -> inner 1 xs
  | _ -> raise Not_a_record_type

type exp =
  | EVar of id
  | EInt of int
  | EBool of bool
  | EBinOp of binOp * exp * exp
  | EAbs of id * exp
  | EApp of exp * exp
  | ELet of id * exp * exp
  | EArray of exp list
  | EArrayGet of exp * idx
  | EArrayModify of exp * idx * exp
  | EIdxAbs of idxvar * exp
  | EIdxApp of exp * idx

type value =
  | VInt of int
  | VBool of bool
  | VProc of id * exp * env
  | VArray of value array
  | VIdxAbs of idxvar * exp

and env = (id, value) Environment.t

and idxenv = (idxvar, idx) Environment.t

type lbenv = (idxty, idx) Environment.t
