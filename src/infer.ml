open Syntax
open PolyRecord
open Subst
module ET = ExplicitlyTyped

exception Not_bound of id

(* counter for fresh type variables *)
let __counter = ref 1

let reset_counter () =
  __counter := 1

let fresh_tyvar () =
  let rval = !__counter in
  __counter := !__counter + 1;
  rval

let forall_of t = Forall ([], t)

(* \mathcal{WK} : (K, T, e) -> (K, S, M, t) *)
let rec infer (kenv : (tyvar, kind) Environment.t) tyenv exp = match exp with
  | EVar x ->
      let Forall (xs, t) =
        try Environment.lookup x tyenv
        with Environment.Not_bound -> raise (Not_bound x)
      in
      let cor_fresh_tyvars_and_kind = List.map (fun (tv, k) -> (tv, fresh_tyvar (), k)) xs in
      let subst =
        cor_fresh_tyvars_and_kind
        |> List.map (fun (tv, s, _) -> (tv, TVar s))
      in
      let kenv' =
        List.fold_left (fun kenv (tv, s, k) ->
          Environment.extend s (apply_subst_to_kind subst k) kenv
        ) kenv cor_fresh_tyvars_and_kind
      in
      let pty =
        apply_subst_to_polyty subst (Forall ([], t))
      in
      (kenv', [], ET.EPolyInst (x, List.map snd subst), pty)
  | EInt i -> (kenv, [], ET.EInt i, forall_of TInt)
  | EBool b -> (kenv, [], ET.EBool b, forall_of TBool)
  | EUnit -> (kenv, [], ET.EUnit, forall_of TUnit)
  | EBinOp (op, e1, e2) ->
      let (kenv1, subst1, e1', Forall (_, t1')) = infer kenv tyenv e1 in
      let (kenv2, subst2, e2', Forall (_, t2')) = infer kenv1 (apply_subst_to_tyenv subst1 tyenv) e2 in
      begin match op with
        | Lt ->
            let eqs = [(t1', TInt); (t2', TInt)] in
            let (kenv3, subst3) = Unify.start eqs kenv2 in
            (kenv3,
             subst1 @ subst2 @ subst3,
             apply_subst_to_exp subst3 (ET.EBinOp (Lt, e1', e2')),
             forall_of TBool)
        | Assign ->
            let eqs = [(t1', TRef t2')] in
            let (kenv3, subst3) = Unify.start eqs kenv2 in
            (kenv3,
             subst1 @ subst2 @ subst3,
             apply_subst_to_exp subst3 (ET.EBinOp (Assign, e1', e2')),
             forall_of TUnit)
        | _ ->
            let eqs = [(t1', TInt); (t2', TInt)] in
            let (kenv3, subst3) = Unify.start eqs kenv2 in
            (kenv3,
             subst1 @ subst2 @ subst3,
             apply_subst_to_exp subst3 (ET.EBinOp (op, e1', e2')),
             forall_of TInt)
      end
  | EIfThenElse (e1, e2, e3) ->
      let (kenv1, subst1, e1', Forall (_, t1')) = infer kenv tyenv e1 in
      let (kenv2, subst2, e2', Forall (_, t2')) = infer kenv1 (apply_subst_to_tyenv subst1 tyenv) e2 in
      let (kenv3, subst3, e3', Forall (_, t3')) = infer kenv2 (apply_subst_to_tyenv (subst1 @ subst2) tyenv) e3 in
      let eqs = [(apply_subst_to_ty subst3 t1', TBool); (t2', t3')] in
      let (kenv4, subst4) = Unify.start eqs kenv3 in
      (kenv4,
       subst1 @ subst2 @ subst3 @ subst4,
       apply_subst_to_exp (subst3 @ subst4) (ET.EIfThenElse (e1', e2', e3')),
       forall_of @@ t3'
      )
  | EUnitAbs e ->
      let (kenv, subst, e', Forall (_, t')) = infer kenv tyenv e in
      (kenv, subst, ET.EUnitAbs e', (forall_of @@ TFun (TUnit, t')))
  | EAbs (x, e) ->
      let tvar = fresh_tyvar () in
      let ty_arg = TVar tvar in
      let kenv' = Environment.extend tvar KUniv kenv in
      let tyenv' = Environment.extend x (Forall ([], ty_arg)) tyenv in
      let (kenv', subst, e', Forall (xs, t')) = infer kenv' tyenv' e in
      (kenv', subst, ET.EAbs (x, apply_subst_to_ty subst ty_arg, e'), (forall_of @@ TFun (apply_subst_to_ty subst ty_arg, t')))
  | EApp (e1, e2) ->
      let (kenv1, subst1, e1', Forall (_, t1')) = infer kenv tyenv e1 in
      let (kenv2, subst2, e2', Forall (_, t2')) = infer kenv1 (apply_subst_to_tyenv subst1 tyenv) e2 in
      let tvar = fresh_tyvar () in
      let ty_ret = TVar tvar in
      let eqs = [(apply_subst_to_ty subst2 t1', TFun (t2', ty_ret))] in
      let kenv' = Environment.extend tvar KUniv kenv2 in
      let (kenv3, subst3) = Unify.start eqs kenv' in
      (kenv3,
       subst1 @ subst2 @ subst3,
       ET.EApp (apply_subst_to_exp (subst2 @ subst3) e1', apply_subst_to_exp subst3 e2'),
       forall_of @@ apply_subst_to_ty subst3 ty_ret)
  | ERecord xs ->
      let (kenv', subst', xs', ts') =
        List.fold_left (fun (kenv, subst, xs, ts) (l, e) ->
          let tyenv' = apply_subst_to_tyenv subst tyenv in
          let (kenv', subst', e', Forall (_, t')) = infer kenv tyenv' e in
          (kenv', subst @ subst', xs @ [l, e'], ts @ [l, t'])
        ) (kenv, [], [], []) xs
      in
      (
        kenv', subst', ET.ERecord (List.map (fun (l, e) -> (l, apply_subst_to_exp subst' e)) xs'),
        forall_of @@ TRecord (List.map (fun (l, t) -> (l, apply_subst_to_ty subst' t)) ts')
      )
  | ERecordGet (e, l) ->
      let (kenv1, subst1, e1', Forall (_, t1')) = infer kenv tyenv e in
      let tv1 = fresh_tyvar () in
      let tv2 = fresh_tyvar () in
      let kenv1' =
        kenv1
        |> Environment.extend tv1 KUniv
        |> Environment.extend tv2 (KRecord [l, TVar tv1])
      in
      let eqs = [TVar tv2, t1'] in
      let (kenv2, subst2) = Unify.start eqs kenv1' in
      (kenv2, subst1 @ subst2,
       apply_subst_to_exp subst2 (ET.ERecordGet (e1', TVar tv2, l)),
       Forall ([], apply_subst_to_ty subst2 (TVar tv1)))
  | ERecordModify (e1, l, e2) ->
      let (kenv1, subst1, e1', Forall (_, t1')) = infer kenv tyenv e1 in
      let (kenv2, subst2, e2', Forall (_, t2')) = infer kenv1 (apply_subst_to_tyenv subst1 tyenv) e2 in
      let tv1 = fresh_tyvar () in
      let tv2 = fresh_tyvar () in
      let kenv2' =
        kenv2
        |> Environment.extend tv1 KUniv
        |> Environment.extend tv2 (KRecord [l, TVar tv1])
      in
      let eqs = [(TVar tv1, t2'); (TVar tv2, apply_subst_to_ty subst2 t1')] in
      let (kenv3, subst3) = Unify.start eqs kenv2' in
      (kenv3, subst1 @ subst2 @ subst3,
       ET.ERecordModify (
        apply_subst_to_exp (subst2 @ subst3) e1',
        apply_subst_to_ty subst3 (TVar tv2),
        l,
        apply_subst_to_exp subst3 e2'
      ), forall_of @@ apply_subst_to_ty subst3 (TVar tv2))
  | ERecordAssign (e1, l, e2) ->
      let (kenv1, subst1, e1', Forall (_, t1')) = infer kenv tyenv e1 in
      let (kenv2, subst2, e2', Forall (_, t2')) = infer kenv1 (apply_subst_to_tyenv subst1 tyenv) e2 in
      let tv1 = fresh_tyvar () in
      let tv2 = fresh_tyvar () in
      let kenv2' =
        kenv2
        |> Environment.extend tv1 KUniv
        |> Environment.extend tv2 (KRecord [l, TVar tv1])
      in
      let eqs = [(TVar tv1, t2'); (TVar tv2, apply_subst_to_ty subst2 t1')] in
      let (kenv3, subst3) = Unify.start eqs kenv2' in
      (kenv3, subst1 @ subst2 @ subst3,
       ET.ERecordAssign (
        apply_subst_to_exp (subst2 @ subst3) e1',
        apply_subst_to_ty subst3 (TVar tv2),
        l,
        apply_subst_to_exp subst3 e2'
      ), forall_of @@ TUnit)
  | ELet (x, e1, e2) ->
      let (kenv1, subst1, e1', Forall (_, t1')) = infer kenv tyenv e1 in
      let tyenv' = apply_subst_to_tyenv subst1 tyenv in
      let (kenv1', pt1) =
        if is_value e1 then closure kenv1 tyenv' t1'
        else cov_closure kenv1 tyenv' t1'
      in
      let tyenv' =
        tyenv
        |> apply_subst_to_tyenv subst1
        |> Environment.extend x pt1
      in
      let (kenv2, subst2, e2', (Forall (_, t2'))) = infer kenv1' tyenv' e2 in
      (
        kenv2, subst1 @ subst2,
        (ET.ELet (x, apply_subst_to_polyty subst2 pt1, apply_subst_to_exp subst2 @@ ET.EPolyGen (e1', pt1), e2')),
        forall_of t2'
      )
  | EStatement (e1, e2) ->
      let (kenv1, subst1, e1', Forall (_, t1')) = infer kenv tyenv e1 in
      let (kenv2, subst2, e2', Forall (_, t2')) = infer kenv1 (apply_subst_to_tyenv subst1 tyenv) e2 in
      (kenv2,
       subst1 @ subst2,
       ET.EStatement (apply_subst_to_exp subst2 e1', e2'),
       forall_of t2')
  | ERef e ->
      let (kenv1, subst1, e', Forall (_, t')) = infer kenv tyenv e in
      (kenv1, subst1, ET.ERef e', forall_of (TRef t'))
  | EDeref e ->
      let (kenv1, subst1, e', Forall (_, t')) = infer kenv tyenv e in
      let tv = fresh_tyvar () in
      let kenv' = Environment.extend tv KUniv kenv1 in
      let eqs = [(t', TRef (TVar tv))] in
      let (kenv2, subst2) = Unify.start eqs kenv' in
      (kenv2, subst1 @ subst2, ET.EDeref (apply_subst_to_exp subst2 e'), forall_of (apply_subst_to_ty subst2 (TVar tv)))

let canonical = function
  | KUniv -> TInt
  | KRecord xs -> TRecord xs

let rec instantiate kenv (Forall (xs, t)) =
  let eftv = eftv_ty kenv t in
  let vacuous =
    Environment.domain kenv
    |> List.filter (fun tv ->
        let kftv = freevar_kind (Environment.lookup tv kenv) in
        not (MySet.member tv (MySet.union kftv eftv))
    )
  in
  let seq = List.map (fun tv ->
    let kenv' = Environment.remove tv kenv in
    let instance = canonical (Environment.lookup tv kenv) in
    let s = (tv, instance) in
    (subst_kenv s kenv', [(tv, instance)])
  ) vacuous
  in
  let (kenv', subst) =
    List.fold_left (fun (kenv, subst) (kenv', subst') ->
      (kenv', subst @ subst')
    ) (kenv, []) seq
  in
  (kenv', subst)

(* entrypoint *)
let start exp =
  reset_counter ();
  let (kenv, _, exp, pty) = infer Environment.empty Environment.empty exp in
  let (kenv', subst, exp', Forall (_, ty)) =
    Misc.until_fix (kenv, [], exp, pty) (fun (kenv, subst, exp, pty) ->
      let (kenv, subst) = instantiate kenv pty in
      (List.fold_right subst_kenv subst kenv, subst, apply_subst_to_exp subst exp, apply_subst_to_polyty subst pty)
    )
  in
  let xs =
    kenv'
    |> Environment.domain
    |> List.rev_map (fun x -> (x, Environment.lookup x kenv))
    |> List.sort_uniq (fun (tv1, k1) (tv2, k2) ->
        if MySet.member tv2 (freevar_kind k1) then 1
        else if MySet.member tv1 (freevar_kind k2) then -1
        else if tv1 = tv2 && k1 = k2 then 0
        else if tv1 > tv2 then 1
        else if tv1 < tv2 then -1
        else 0
      )
  in
  (exp', kenv', Forall (xs, ty))
