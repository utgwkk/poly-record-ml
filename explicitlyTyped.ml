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
        |> List.map (fun (i, k) -> Printf.sprintf "\"%d\", %s" i (string_of_kind k))
      in
      Printf.sprintf "Forall ([%s], %s)" (String.concat "; " xs') (string_of_ty t)
