open Syntax

type ty =
  | TVar of tyvar
  | TInt
  | TBool
  | TFun of ty * ty
  | TRecord of (label * ty) list

and kind =
  | KUniv
  | KRecord of (label * ty) list

type polyty = Forall of (tyvar * kind) list * ty

type exp =
  | EPolyInst of id * ty list
  | EInt of int
  | EBool of bool
  | EBinOp of binOp * exp * exp
  | EIfThenElse of exp * exp * exp
  | EAbs of id * exp
  | EApp of exp * exp
  | ELet of id * exp * exp
  | ERecord of (label * exp) list
  | ERecordGet of exp * label
  | ERecordModify of exp * label * exp

let rec ty_eq t1 t2 = match t1, t2 with
  | TVar i1, TVar i2 -> i1 = i2
  | TInt, TInt -> true
  | TBool, TBool -> true
  | TFun (t1, t1r), TFun (t2, t2r) ->
      ty_eq t1 t2 && ty_eq t1r t2r
  | TRecord xs, TRecord ys -> begin
      try List.fold_left2 (fun b (l1, x) (l2, y) -> b && l1 = l2 && ty_eq x y) true xs ys
      with _ -> false
    end
  | _ -> false

and kind_eq k1 k2 = match k1, k2 with
  | KUniv, KUniv -> true
  | KRecord xs, KRecord ys -> begin
      try List.fold_left2 (fun b (l1, x) (l2, y) -> b && l1 = l2 && ty_eq x y) true xs ys
      with _ -> false
    end
  | _ -> false

and polyty_eq (Forall (xs, t1)) (Forall (ys, t2)) =
  (
    try List.fold_left2 (fun b (v1, x) (v2, y) -> b && v1 = v2 && kind_eq x y) true xs ys
    with _ -> false
  ) && ty_eq t1 t2

