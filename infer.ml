open Syntax
open PolyRecord
module ET = ExplicitlyTyped

exception Not_bound of id

(* E *)
type eqs = (ty * ty) list

(* S *)
type 'a subst = (tyvar * 'a) list

(* ty subst -> eqs *)
let eqs_of_subst s = List.map (fun (tv, a) -> (TVar tv, a)) s

(* [ty/tyvar]ty *)
let rec subst_ty t (tv, ty) = match t with
  | TVar tv' -> if tv = tv' then ty else TVar tv'
  | TFun (t1, t2) ->
      TFun (subst_ty t1 (tv, ty), subst_ty t2 (tv, ty))
  | TRecord xs ->
      let xs' =
        xs
        |> List.map (fun (l, t) -> (l, subst_ty t (tv, ty)))
      in TRecord xs'
  | t -> t

(* [ty/tyvar]S *)
let subst_subs s subst = List.map (fun (tv, t) -> (tv, subst_ty t s)) subst

(* [ty/tyvar]k *)
let subst_kind s = function
  | KUniv -> KUniv
  | KRecord xs ->
      let xs' =
        xs
        |> List.map (fun (l, t) -> (l, subst_ty t s))
      in KRecord xs'

(* [ty/tyvar]K *)
let subst_kenv s kenv = Environment.map (subst_kind s) kenv

(* S(ty) *)
let apply_subst_to_ty subst t = List.fold_left (fun t s -> subst_ty t s) t subst

(* S(T) *)
let apply_subst_to_tyenv s tyenv =
  List.fold_left (fun env s -> Environment.map (fun (Forall (xs, t)) -> Forall (xs, subst_ty t s)) env) tyenv s

(* [ty/tyvar]E *)
let subst_eqs subst eqs =
  List.map (fun (t1, t2) -> (apply_subst_to_ty subst t1, apply_subst_to_ty subst t2)) eqs

(* レコードのフィールド名の和集合をとる *)
let union_record xs ys =
  let xs' =
    xs
    |> List.map fst
    |> List.sort compare
  in
  let ys' =
    ys
    |> List.map fst
    |> List.sort compare
  in
  let rec inner xs ys = match xs with
    | [] -> ys
    | lx :: tx -> match ys with
        | [] -> xs
        | ly :: ty ->
            if lx = ly then lx :: inner tx ty
            else if lx > ly then ly :: inner (lx :: tx) ty
            else lx :: inner tx (ly :: ty)
  in inner xs' ys'

(* レコードのフィールド名の積集合をとる *)
let intersect_record xs ys =
  let xs' =
    xs
    |> List.map fst
    |> List.sort compare
  in
  let ys' =
    ys
    |> List.map fst
    |> List.sort compare
  in
  let rec inner xs ys = match xs with
    | [] -> []
    | lx :: tx -> match ys with
        | [] -> []
        | ly :: ty ->
            if lx = ly then lx :: inner tx ty
            else if lx > ly then inner (lx :: tx) ty
            else inner tx (ly :: ty)
  in inner xs' ys'

(* dom(F1) \subseteq dom(F2) *)
let dom_is_subset xs ys =
  let xs' = List.map fst xs in
  let ys' = List.map fst ys in
  List.for_all (fun x -> List.mem x ys') xs'

(* dom(F1) = dom(F2) *)
let dom_eq xs ys =
  let xs' = List.map fst xs in
  let ys' = List.map fst ys in
  List.for_all (fun x -> List.mem x ys') xs' &&
  List.for_all (fun y -> List.mem y xs') ys'

(* tyvar \in FTV(ty) *)
let rec ftv tv = function
  | TVar tv' -> tv = tv'
  | TFun (t1, t2) -> ftv tv t1 || ftv tv t2
  | TRecord xs -> List.exists (fun (_, t) -> ftv tv t) xs
  | _ -> false

exception Unification_failed of string

(* \mathcal{U} *)
let rec unify eqs kenv subst ksubst =
  match eqs with
  | [] -> (kenv, subst)
  | (t1, t2) :: rest ->
      if ty_eq t1 t2 then unify rest kenv subst ksubst (* (I) *)
      else match t1, t2 with
      | TRecord xs, TRecord ys -> (* (V) *)
          if dom_eq xs ys then
            let eqs' =
              List.map (fun (l, _) -> (List.assoc l xs, List.assoc l ys)) xs @ rest
            in
            unify eqs' kenv subst ksubst
          else raise (Unification_failed "not the same domain record")
      | TFun (t11, t12), TFun (t21, t22) -> (* (IX) *)
          let eqs' = (t11, t21) :: (t12, t22) :: rest in
          unify eqs' kenv subst ksubst
      | TVar tv1, TVar tv2 ->
          let k1 = Environment.lookup tv1 kenv in
          let k2 = Environment.lookup tv2 kenv in
          begin match k1, k2 with
          | KRecord xs, KRecord ys -> (* (III) *)
              let isect = intersect_record xs ys in
              let constrs = List.map (fun x -> (List.assoc x xs, List.assoc x ys)) isect in
              let eqs' = constrs @ subst_eqs [(tv1, TVar tv2)] rest in
              let uni = union_record xs ys in
              let k' =
                KRecord (List.map (fun x -> (x, List.assoc x (xs @ ys))) uni)
                |> subst_kind (tv1, TVar tv2)
              in
              let kenv' =
                kenv
                |> subst_kenv (tv1, TVar tv2)
                |> Environment.extend tv2 k'
              in
              let subst' = (tv1, TVar tv2) :: subst_subs (tv1, TVar tv2) subst in
              let ksubst' =
                ksubst
                |> subst_kenv (tv1, TVar tv2)
                |> Environment.extend tv1 (KRecord xs)
              in
              unify eqs' kenv' subst' ksubst'
          | KRecord _, KUniv -> (* (II)' *)
              unify ((TVar tv2, TVar tv1):: rest) kenv subst ksubst
          | KUniv, _ -> (* (II)' *)
              let eqs' = subst_eqs [(tv1, TVar tv2)] rest in
              let kenv' =
                kenv
                |> subst_kenv (tv1, TVar tv2)
              in
              let subst' = (tv1, TVar tv2) :: subst_subs (tv1, TVar tv2) subst in
              let ksubst' =
                ksubst
                |> subst_kenv (tv1, TVar tv2)
                |> Environment.extend tv1 KUniv
              in
              unify eqs' kenv' subst' ksubst'
          end
      | TVar tv, TRecord ys -> (* (IV) *)
          let k = Environment.lookup tv kenv in
          begin match k with
          | KUniv ->
              let eqs' = subst_eqs [(tv, TRecord ys)] rest in
              let kenv' = subst_kenv (tv, TRecord ys) kenv in
              let subst' = (tv, TRecord ys) :: subst_subs (tv, TRecord ys) subst in
              let ksubst' =
                kenv
                |> subst_kenv (tv, TRecord ys)
                |> Environment.extend tv (KRecord ys)
              in
              unify eqs' kenv' subst' ksubst'
          | KRecord xs ->
              let eqs' =
                List.map (fun (l, k) ->
                  try (k, List.assoc l ys)
                  with Not_found ->
                    raise (Unification_failed (Printf.sprintf "field '%s' not found on %s" l (string_of_kind (KRecord ys))))
                ) xs @ rest
                |> subst_eqs [(tv, TRecord ys)]
              in
              let kenv' = subst_kenv (tv, TRecord ys) kenv in
              let subst' = (tv, TRecord ys) :: subst_subs (tv, TRecord ys) subst in
              let ksubst' =
                kenv
                |> subst_kenv (tv, TRecord ys)
                |> Environment.extend tv (KRecord xs)
              in
              if dom_is_subset xs ys && not (ftv tv (TRecord xs))
              then unify eqs' kenv' subst' ksubst'
              else raise (Unification_failed "not a subset record")
          end
      | TVar tv, t -> (* (II) *)
          if ftv tv t then
            raise (Unification_failed (Printf.sprintf "free variable %d occurs inside %s" tv (string_of_ty t)))
          else
            let k = Environment.lookup tv kenv in
            begin match k with
                | KUniv ->
                    let eqs' = subst_eqs [(tv, t)] rest in
                    let kenv' = subst_kenv (tv, t) kenv in
                    let subst' = (tv, t) :: subst_subs (tv, t) subst in
                    let ksubst' = subst_kenv (tv, t) ksubst in
                    unify eqs' kenv' subst' ksubst'
                | _ -> raise (Unification_failed "unify failed")
            end
      | t, TVar tv -> (* (II) *)
          let eqs' = (TVar tv, t) :: rest in
          unify eqs' kenv subst ksubst
      | _ -> raise (Unification_failed (Printf.sprintf "unify failed: (%s, %s)" (string_of_ty t1) (string_of_ty t2)))

let start_unify eqs kenv = unify eqs kenv [] Environment.empty

(* S(M) *)
let rec apply_subst_to_exp subs = function
  | ET.EPolyInst (x, ts) ->
      ET.EPolyInst (x, List.map (apply_subst_to_ty subs) ts)
  | ET.EBinOp (op, e1, e2) ->
      ET.EBinOp (op, apply_subst_to_exp subs e1, apply_subst_to_exp subs e2)
  | ET.EIfThenElse (e1, e2, e3) ->
      ET.EIfThenElse (
        apply_subst_to_exp subs e1,
        apply_subst_to_exp subs e2,
        apply_subst_to_exp subs e3
      )
  | ET.EAbs (x, t, e) ->
      ET.EAbs (x, apply_subst_to_ty subs t, apply_subst_to_exp subs e)
  | ET.EApp (e1, e2) ->
      ET.EApp (
        apply_subst_to_exp subs e1,
        apply_subst_to_exp subs e2
      )
  | ET.EPolyGen (e, pt) ->
      ET.EPolyGen (
        apply_subst_to_exp subs e,
        apply_subst_to_polyty subs pt
      )
  | ET.ELet (x, pt, e1, e2) ->
      ET.ELet (
        x,
        apply_subst_to_polyty subs pt,
        apply_subst_to_exp subs e1,
        apply_subst_to_exp subs e2
      )
  | ET.ERecord xs ->
      let xs' =
        xs
        |> List.map (fun (l, e) -> (l, apply_subst_to_exp subs e))
      in ET.ERecord xs'
  | ET.ERecordGet (e, t, l) ->
      ET.ERecordGet (
        apply_subst_to_exp subs e,
        apply_subst_to_ty subs t,
        l
      )
  | ET.ERecordModify (e1, t, l, e2) ->
      ET.ERecordModify (
        apply_subst_to_exp subs e1,
        apply_subst_to_ty subs t,
        l,
        apply_subst_to_exp subs e2
      )
  | e -> e

(* S(k) *)
and apply_subst_to_kind subs = function
  | KUniv -> KUniv
  | KRecord xs ->
      let xs' =
        xs
        |> List.map (fun (l, t) -> (l, apply_subst_to_ty subs t))
      in KRecord xs'

(* S(tv) *)
and apply_subst_to_tyvar subs tv =
  List.fold_left (fun tv (tv1, t) ->
    match t with
      | TVar tv2 -> if tv = tv1 then tv2 else tv
      | _ -> tv
  ) tv subs

(* S(polyty) *)
and apply_subst_to_polyty subs = function
  | Forall (xs, t) ->
      let xs' =
        xs
        |> List.map (fun (tv, k) -> (apply_subst_to_tyvar subs tv, apply_subst_to_kind subs k))
      in Forall (xs', apply_subst_to_ty subs t)

(* FTV(ty) *)
let rec freevar_ty = function
  | TVar tv -> MySet.singleton tv
  | TFun (t1, t2) -> MySet.union (freevar_ty t1) (freevar_ty t2)
  | TRecord xs ->
      List.fold_left (fun ftv (_, t) ->
        MySet.union ftv (freevar_ty t)
      ) MySet.empty xs
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

let rec until_fix old f =
  let next = f old in
  if old = next then old
  else until_fix next f

(* EFTV(kenv, ty) *)
let eftv_ty kenv ty =
  let ftv_ty = freevar_ty ty in
  until_fix ftv_ty (fun eftv ->
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
  until_fix ftv_ty (fun eftv ->
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
    tyenv
    |> Environment.domain
    |> List.map (fun x ->
        Environment.lookup x tyenv
    )
    |> List.map (eftv_polyty kenv)
    |> MySet.from_list
    |> MySet.bigunion
  in
  until_fix ftv_tyenv (fun eftv ->
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
      let subst = List.map (fun (tv, _) -> (tv, TVar (fresh_tyvar ()))) xs in
      let kenv' =
        List.fold_left2 (fun kenv (tv, TVar s) (_, k) ->
          Environment.extend s (apply_subst_to_kind subst k) kenv
        ) kenv subst xs
      in
      let pty =
        apply_subst_to_polyty subst (Forall ([], t))
      in
      (kenv', [], ET.EPolyInst (x, List.map snd subst), pty)
  | EInt i -> (kenv, [], ET.EInt i, forall_of TInt)
  | EBool b -> (kenv, [], ET.EBool b, forall_of TBool)
  | EBinOp (op, e1, e2) ->
      let (kenv1, subst1, e1', Forall (_, t1')) = infer kenv tyenv e1 in
      let (kenv2, subst2, e2', Forall (_, t2')) = infer kenv1 (apply_subst_to_tyenv subst1 tyenv) e2 in
      let eqs = [(t1', TInt); (t2', TInt)] in
      let (kenv3, subst3) = start_unify eqs kenv2 in
      begin match op with
        | Lt ->
            (kenv3,
             subst1 @ subst2 @ subst3,
             apply_subst_to_exp subst3 (ET.EBinOp (Lt, e1', e2')),
             forall_of TBool)
        | _ ->
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
      let (kenv4, subst4) = start_unify eqs kenv3 in
      (kenv4,
       subst1 @ subst2 @ subst3 @ subst4,
       apply_subst_to_exp (subst3 @ subst4) (ET.EIfThenElse (e1', e2', e3')),
       forall_of @@ t3'
      )
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
      let (kenv3, subst3) = start_unify eqs kenv' in
      (kenv3,
       subst1 @ subst2 @ subst3,
       ET.EApp (apply_subst_to_exp (subst2 @ subst3) e1', apply_subst_to_exp subst3 e2'),
       forall_of @@ apply_subst_to_ty subst3 ty_ret)
  | ERecord xs ->
      let xs = List.sort compare xs in
      let (kenv', subst', xs', ts') =
        List.fold_left (fun (kenv, subst, xs, ts) (l, e) ->
          let tyenv' = apply_subst_to_tyenv subst tyenv in
          let (kenv', subst', e', Forall (_, t')) = infer kenv tyenv' e in
          (kenv', subst @ subst', xs @ [l, e'], ts @ [l, t'])
        ) (kenv, [], [], []) xs
      in
      let ts' = List.sort compare ts' in
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
      let (kenv2, subst2) = start_unify eqs kenv1' in
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
      let (kenv3, subst3) = start_unify eqs kenv2' in
      (kenv3, subst1 @ subst2 @ subst3,
       ET.ERecordModify (
        apply_subst_to_exp (subst2 @ subst3) e1',
        apply_subst_to_ty subst3 (TVar tv2),
        l,
        apply_subst_to_exp subst3 e2'
      ), forall_of @@ apply_subst_to_ty subst3 (TVar tv2))
  | ELet (x, e1, e2) ->
      let (kenv1, subst1, e1', Forall (_, t1')) = infer kenv tyenv e1 in
      let (kenv1', pt1) = closure kenv1 (apply_subst_to_tyenv subst1 tyenv) t1' in
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

let canonical = function
  | KUniv -> TInt
  | KRecord xs -> TRecord xs

let rec instantiate kenv (Forall (xs, t)) =
  let eftv = eftv_ty kenv t in
  let vacuous =
    Environment.domain kenv
    |> List.filter (fun tv -> not (MySet.member tv eftv))
    |> List.filter (fun tv -> not (MySet.member tv (freevar_kind (Environment.lookup tv kenv))))
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
  let (kenv', subst, exp', _) =
    until_fix (kenv, [], exp, pty) (fun (kenv, subst, exp, pty) ->
      let (kenv, subst) = instantiate kenv pty in
      (List.fold_right subst_kenv subst kenv, subst, apply_subst_to_exp subst exp, apply_subst_to_polyty subst pty)
    )
  in
  (exp', kenv', pty)
