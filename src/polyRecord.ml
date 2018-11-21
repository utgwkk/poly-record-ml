open Syntax

type ty =
  | TVar of tyvar
  | TInt
  | TBool
  | TUnit
  | TFun of ty * ty
  | TRecord of ty record
  | TRef of ty
  | TIdxFun of (label * ty) list * ty

and kind =
  | KUniv
  | KRecord of ty record

type polyty = Forall of (tyvar * kind) list * ty

type exp =
  | EVar of id
  | EInt of int
  | EBool of bool
  | EUnit
  | EBinOp of binOp * exp * exp
  | EIfThenElse of exp * exp * exp
  | EAbs of id * exp
  | EUnitAbs of exp
  | EApp of exp * exp
  | ELet of id * exp * exp
  | ERecord of exp record
  | ERecordGet of exp * label
  | ERecordModify of exp * label * exp
  | ERecordAssign of exp * label * exp
  | EStatement of exp * exp
  | ERef of exp
  | EDeref of exp

let rec string_of_ty = function
  | TVar i -> "TVar " ^ string_of_int i
  | TInt -> "TInt"
  | TBool -> "TBool"
  | TUnit -> "TUnit"
  | TFun (t1, t2) -> Printf.sprintf "TFun (%s, %s)" (string_of_ty t1) (string_of_ty t2)
  | TRecord xs ->
      let xs' =
        xs
        |> List.map (fun (l, t) -> Printf.sprintf "\"%s\", %s" l (string_of_ty t))
      in
      Printf.sprintf "TRecord [%s]" (String.concat "; " xs')
  | TRef t -> Printf.sprintf "TRef (%s)" (string_of_ty t)
  | TIdxFun (xs, t) ->
      let xs' =
        xs
        |> List.map (fun (l, t) -> Printf.sprintf "\"%s\", %s" l (string_of_ty t))
      in
      Printf.sprintf "TIdxFun ([%s], %s)" (String.concat "; " xs') (string_of_ty t)

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

let pp_polyty (Forall (bs, t)) =
  let collect_tyvar_polyty (Forall (xs, t)) =
    xs |> List.map fst
  in
  let rec tyvar_map count = function
    | [] -> []
    | h :: t -> (h, "t" ^ string_of_int count) :: tyvar_map (count + 1) t
  in
  let tyvars = tyvar_map 0 (collect_tyvar_polyty (Forall (bs, t)))
  in
  let rec pp_ty t =
    let rec pp_ty' t =
      match t with
      | TInt -> "int"
      | TBool -> "bool"
      | TUnit -> "unit"
      | TVar i -> "'" ^ List.assoc i tyvars
      | TFun (a, b) -> begin match (a, b) with
          | TFun _, _ -> Printf.sprintf "(%s) -> %s" (pp_ty' a) (pp_ty' b)
          | _ -> Printf.sprintf "%s -> %s" (pp_ty' a) (pp_ty' b)
        end
      | TRecord xs ->
          let xs' =
            xs
            |> List.map (fun (l, t) -> Printf.sprintf "%s : %s" l (pp_ty t))
          in
          Printf.sprintf "{%s}" (String.concat ", " xs')
      | TRef t -> begin match t with
          | TFun _ -> Printf.sprintf "(%s) ref" (pp_ty' t)
          | _ -> Printf.sprintf "%s ref" (pp_ty' t)
      end
      | TIdxFun (xs, t) ->
          let xs' =
            xs
            |> List.map (fun (l, t) -> Printf.sprintf "idx(%s, %s)" l (pp_ty t))
          in
          Printf.sprintf "%s => %s" (String.concat " => " xs') (string_of_ty t)
    in pp_ty' t
  and pp_kind = function
    | KUniv -> ""
    | KRecord xs -> "::#" ^ pp_ty (TRecord xs)
  in
  match bs with
  | [] -> pp_ty t
  | _ ->
      let bs' =
        bs
        |> List.map (fun (v, k) -> Printf.sprintf "%s%s" (pp_ty (TVar v)) (pp_kind k))
      in
      Printf.sprintf "forall %s. %s" (String.concat ", " bs') (pp_ty t)

let rec string_of_exp = function
  | EVar x ->
      Printf.sprintf "EVar \"%s\"" x
  | EInt i -> "EInt " ^ string_of_int i
  | EBool b -> "EBool " ^ string_of_bool b
  | EUnit -> "EUnit"
  | EBinOp (op, e1, e2) ->
      let opstr = string_of_binOp op
      in Printf.sprintf "EBinOp (%s, %s, %s)" opstr (string_of_exp e1) (string_of_exp e2)
  | EIfThenElse (e1, e2, e3) ->
      Printf.sprintf "EIfThenElse (%s, %s, %s)" (string_of_exp e1) (string_of_exp e2) (string_of_exp e3)
  | EAbs (x, e) ->
      Printf.sprintf "EAbs (\"%s\", %s)" x (string_of_exp e)
  | EUnitAbs e ->
      Printf.sprintf "EUnitAbs (%s)" (string_of_exp e)
  | EApp (e1, e2) ->
      Printf.sprintf "EApp (%s, %s)" (string_of_exp e1) (string_of_exp e2)
  | ELet (x, e1, e2) ->
      Printf.sprintf "ELet (\"%s\", %s, %s)" x (string_of_exp e1) (string_of_exp e2)
  | ERecord xs ->
      let xs' = List.map (fun (l, e) -> Printf.sprintf "(\"%s\", %s)" l (string_of_exp e)) xs in
      Printf.sprintf "ERecord [%s]" (String.concat "; " xs')
  | ERecordGet (e, l) ->
      Printf.sprintf "ERecordGet (%s, \"%s\")" (string_of_exp e)  l
  | ERecordModify (e1, l, e2) ->
      Printf.sprintf "ERecordModify (%s, \"%s\", %s)" (string_of_exp e1) l (string_of_exp e2)
  | ERecordAssign (e1, l, e2) ->
      Printf.sprintf "ERecordAssign (%s, \"%s\", %s)" (string_of_exp e1) l (string_of_exp e2)
  | EStatement (e1, e2) ->
      Printf.sprintf "EStatement (%s, %s)" (string_of_exp e1) (string_of_exp e2)
  | ERef e ->
      Printf.sprintf "ERef (%s)" (string_of_exp e)
  | EDeref e ->
      Printf.sprintf "EDeref (%s)" (string_of_exp e)

(* tyvar \in FTV(ty) *)
let rec ftv tv = function
  | TVar tv' -> tv = tv'
  | TFun (t1, t2) -> ftv tv t1 || ftv tv t2
  | TRecord xs -> List.exists (fun (_, t) -> ftv tv t) xs
  | TRef t -> ftv tv t
  | _ -> false

(* FTV(ty) *)
let rec freevar_ty = function
  | TVar tv -> MySet.singleton tv
  | TFun (t1, t2) -> MySet.union (freevar_ty t1) (freevar_ty t2)
  | TRecord xs ->
      List.fold_left (fun ftv (_, t) ->
        MySet.union ftv (freevar_ty t)
      ) MySet.empty xs
  | TRef t -> freevar_ty t
  | _ -> MySet.empty

(* FTV(k) *)
let freevar_kind = function
  | KUniv -> MySet.empty
  | KRecord xs -> freevar_ty (TRecord xs)

(* FTV(polyty) *)
let freevar_polyty (Forall (xs, t)) =
  List.fold_left (fun (fvs, bs) (tv, k) ->
    let fv_k = freevar_kind k in
    let bs' = MySet.insert tv bs in
    (MySet.union fv_k (MySet.diff (freevar_ty t) bs'), bs')
  ) (freevar_ty t, MySet.empty) xs
  |> fst

(* FTV(tyenv) *)
let freevar_tyenv tyenv =
  Environment.fold_right (fun pt ftv ->
    MySet.insert (freevar_polyty pt) ftv
  ) tyenv MySet.empty
  |> MySet.bigunion

(* EFTV(kenv, ty) *)
let eftv_ty kenv ty =
  let ftv_ty = freevar_ty ty in
  Misc.until_fix ftv_ty (fun eftv ->
    eftv
    |> MySet.map (fun tv ->
        let k = Environment.lookup tv kenv in
        freevar_kind k
      )
    |> MySet.bigunion
    |> MySet.union eftv
  )

(* EFTV(kenv, polyty) *)
let eftv_polyty kenv pt =
  let ftv_ty = freevar_polyty pt in
  Misc.until_fix ftv_ty (fun eftv ->
    eftv
    |> MySet.map (fun tv ->
        let k = try Environment.lookup tv kenv with _ -> KUniv in
        freevar_kind k
      )
    |> MySet.bigunion
    |> MySet.union eftv
  )

(* EFTV(kenv, tyenv) *)
let eftv_tyenv kenv tyenv =
  let ftv_tyenv =
    Environment.fold_right (fun pt set ->
      MySet.union set (eftv_polyty kenv pt)
    ) tyenv MySet.empty
  in
  Misc.until_fix ftv_tyenv (fun eftv ->
    eftv
    |> MySet.map (fun tv ->
        let k = try Environment.lookup tv kenv with _ -> KUniv in
        freevar_kind k
      )
    |> MySet.bigunion
    |> MySet.union eftv
  )

(* Cls(K, T, t) = (K', pt) *)
let closure kenv tyenv ty =
  let ids =
    MySet.diff (eftv_ty kenv ty) (eftv_tyenv kenv tyenv)
    |> MySet.to_list
  in
  let xs =
    ids
    |> List.map (fun x -> (x, Environment.lookup x kenv))
  in
  let kenv' = List.fold_left (fun kenv id ->
    Environment.remove id kenv
  ) kenv ids in
  (kenv', Forall (xs, ty))

let rec is_value = function
  | EVar _
  | EInt _
  | EBool _
  | EBinOp _
  | EAbs _
  | EUnitAbs _
    -> true
  | ERecord xs ->
      List.for_all (fun (_, e) -> is_value e) xs
  | EDeref e
  | ERecordGet (e, _)
  | ERecordModify (_, _, e)
  | EStatement (_, e) -> is_value e
  | EIfThenElse (e1, e2, e3) -> is_value e1 && is_value e2 && is_value e3
  | _ -> false
