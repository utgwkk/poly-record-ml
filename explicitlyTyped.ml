open Syntax

type idx = Implementation.idx

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
  | EAbs of id * ty * exp
  | EApp of exp * exp
  | EPolyGen of exp * polyty
  | ELet of id * polyty * exp * exp
  | ERecord of (label * exp) list
  | ERecordGet of exp * ty * label
  | ERecordModify of exp * ty * label * exp

let rec string_of_ty = function
  | TVar i -> "TVar " ^ string_of_int i
  | TInt -> "TInt"
  | TBool -> "TBool"
  | TFun (t1, t2) -> Printf.sprintf "TFun (%s, %s)" (string_of_ty t1) (string_of_ty t2)
  | TRecord xs ->
      let xs' =
        xs
        |> List.map (fun (l, t) -> Printf.sprintf "\"%s\", %s" l (string_of_ty t))
      in
      Printf.sprintf "TRecord [%s]" (String.concat "; " xs')

let rec string_of_kind = function
  | KUniv -> "KUniv"
  | KRecord xs ->
      let xs' =
        xs
        |> List.map (fun (l, t) -> Printf.sprintf "\"%s\", %s" l (string_of_ty t))
      in
      Printf.sprintf "KRecord [%s]" (String.concat "; " xs')

let string_of_polyty = function
  | Forall (xs, t) ->
      let xs' =
        xs
        |> List.map (fun (i, k) -> Printf.sprintf "%d, %s" i (string_of_kind k))
      in
      Printf.sprintf "Forall ([%s], %s)" (String.concat "; " xs') (string_of_ty t)

let rec string_of_exp = function
  | EPolyInst (x, xs) ->
      let xs' =
        xs
        |> List.map string_of_ty
      in
      Printf.sprintf "PolyInst (%s, [%s])" x (String.concat "; " xs')
  | EInt i -> "EInt " ^ string_of_int i
  | EBool b -> "EBool " ^ string_of_bool b
  | EBinOp (op, e1, e2) ->
      let opstr = match op with
      | Plus -> "Plus"
      | Mult -> "Mult"
      | Lt -> "Lt"
      in Printf.sprintf "EBinOp (%s, %s, %s)" opstr (string_of_exp e1) (string_of_exp e2)
  | EIfThenElse (e1, e2, e3) ->
      Printf.sprintf "EIfThenElse (%s, %s, %s)" (string_of_exp e1) (string_of_exp e2) (string_of_exp e3)
  | EAbs (x, t, e) ->
      Printf.sprintf "EAbs (\"%s\", %s, %s)" x (string_of_ty t) (string_of_exp e)
  | EApp (e1, e2) ->
      Printf.sprintf "EApp (%s, %s)" (string_of_exp e1) (string_of_exp e2)
  | EPolyGen (e, pt) ->
      Printf.sprintf "EPolyGen (%s, %s)" (string_of_exp e) (string_of_polyty pt)
  | ELet (x, pt, e1, e2) ->
      Printf.sprintf "ELet (\"%s\", %s, %s, %s)" x (string_of_polyty pt) (string_of_exp e1) (string_of_exp e2)
  | ERecord xs ->
      let xs' = List.map (fun (l, e) -> Printf.sprintf "(\"%s\", %s)" l (string_of_exp e)) xs in
      Printf.sprintf "ERecord [%s]" (String.concat "; " xs')
  | ERecordGet (e, t, l) ->
      Printf.sprintf "ERecordGet (%s, %s, \"%s\")" (string_of_exp e) (string_of_ty t) l
  | ERecordModify (e1, t, l, e2) ->
      Printf.sprintf "ERecordModify (%s, %s, \"%s\", %s)" (string_of_exp e1) (string_of_ty t) l (string_of_exp e2)
