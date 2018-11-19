open ExplicitlyTyped
open PolyRecord

exception Typecheck_failed
exception Kindcheck_failed

(* [t/tvar] *)
let rec substitute tvar t = function
  | TVar tv -> if tvar = tv then t else TVar tv
  | TFun (t1, t2) -> TFun (substitute tvar t t1, substitute tvar t t2)
  | TRecord ts ->
      let ts' =
        ts
        |> List.map (fun (l, ty) -> (l, substitute tvar t ty))
      in TRecord ts'
  | ty -> ty (* base type *)

let rec instantiate (Forall (ys, t)) = function
  | [] -> Forall (ys, t)
  | ty :: tl -> begin match ys with
      | [] -> Forall (ys, t)
      | (tv, _)::rest -> instantiate (Forall (rest, substitute tv ty t)) tl
  end

let forall_of t = Forall ([], t)

(* K |- t :: {{ l : ? }} *)
let kind_check kenv t l = match t with
  | TVar i ->
      let k = Environment.lookup i kenv in
      begin match k with
      | KUniv -> raise Kindcheck_failed
      | KRecord xs ->
        List.fold_left (fun t (l', t') ->
          match t with
          | Some _ -> t
          | None -> if l = l' then Some t' else t
        ) None xs
      end
  | TRecord xs ->
      List.fold_left (fun t (l', t') ->
        match t with
        | Some _ -> t
        | None -> if l = l' then Some t' else t
      ) None xs
  | _ -> raise Kindcheck_failed

(* K, T |- M : ? *)
let rec type_check kenv tyenv = function
  | EPolyInst (x, xs) ->
      let pt = Environment.lookup x tyenv in
       instantiate pt xs
  | EInt _ -> forall_of TInt
  | EBool _ -> forall_of TBool
  | EUnit -> forall_of TUnit
  | EBinOp (op, e1, e2) ->
      let (Forall (_, t1)) = type_check kenv tyenv e1 in
      let (Forall (_, t2)) = type_check kenv tyenv e2 in
      begin match op with
        | Plus -> if t1 = TInt && t2 = TInt then forall_of TInt
                  else raise Typecheck_failed
        | Mult -> if t1 = TInt && t2 = TInt then forall_of TInt
                  else raise Typecheck_failed
        | Lt -> if t1 = TInt && t2 = TInt then forall_of TBool
                else raise Typecheck_failed
      end
  | EIfThenElse (e1, e2, e3) ->
      let (Forall (_, t1)) = type_check kenv tyenv e1 in
      let pt1 = type_check kenv tyenv e2 in
      let pt2 = type_check kenv tyenv e3 in
      begin match t1 with
        | TBool -> if pt1 = pt2 then pt1 else raise Typecheck_failed
        | _ -> raise Typecheck_failed
      end
  | EAbs (x, t, e) ->
      let tyenv' = Environment.extend x (Forall ([], t)) tyenv in
      let (Forall (xs, t')) = type_check kenv tyenv' e in
      Forall (xs, TFun (t, t'))
  | EUnitAbs e ->
      let (Forall (xs, t')) = type_check kenv tyenv e in
      Forall (xs, TFun (TUnit, t'))
  | EApp (e1, e2) ->
      let (Forall (_, t1)) = type_check kenv tyenv e1 in
      let (Forall (_, t2)) = type_check kenv tyenv e2 in
      begin match t1 with
        | TFun (t_arg, t_ret) ->
            if t_arg = t2 then forall_of t_ret else raise Typecheck_failed
        | _ -> raise Typecheck_failed
      end
  | EPolyGen (e, Forall (xs, t)) ->
      let kenv' =
        List.fold_left (fun env (t, k) ->
          Environment.extend t k env
        ) kenv xs
      in
      let pt = Forall (xs, t) in
      let (Forall (_, t')) = type_check kenv' tyenv e in
      let (kenv'', pt') = closure kenv' tyenv t' in
      if kenv <> kenv'' || pt <> pt' then raise Typecheck_failed
      else pt
  | ELet (x, pt, e1, e2) ->
      let pt' = type_check kenv tyenv e1 in
      if pt <> pt' then raise Typecheck_failed
      else
        let tyenv' = Environment.extend x pt' tyenv in
        type_check kenv tyenv' e2
  | ERecord xs ->
      let xs' =
        xs
        |> List.map (fun (l, t) ->
            let (Forall (_, t')) = type_check kenv tyenv t in
            (l, t')
        ) in
      forall_of @@ TRecord xs'
  | ERecordGet (e, t, l) ->
      let (Forall (_, t')) = type_check kenv tyenv e in
      begin match kind_check kenv t' l with
        | Some t -> forall_of t
        | None -> raise Typecheck_failed
      end
  | ERecordModify (e1, t, l, e2) ->
      let (Forall (xs, t')) = type_check kenv tyenv e1 in
      begin match kind_check kenv t' l with
        | Some t1 ->
            let (Forall (_, t2)) = type_check kenv tyenv e2 in
            if t1 = t2 then Forall (xs, t') else raise Typecheck_failed
        | None -> raise Typecheck_failed
      end
  | ERecordAssign (e1, t, l, e2) ->
      let (Forall (xs, t')) = type_check kenv tyenv e1 in
      begin match kind_check kenv t' l with
        | Some t1 ->
            let (Forall (_, t2)) = type_check kenv tyenv e2 in
            if t1 = t2 then Forall (xs, TUnit) else raise Typecheck_failed
        | None -> raise Typecheck_failed
      end
  | EStatement (e1, e2) ->
      ignore (type_check kenv tyenv e1);
      let (Forall (_, t2)) = type_check kenv tyenv e2 in
      forall_of t2

(* entrypoint *)
let start kenv exp = type_check kenv Environment.empty exp
