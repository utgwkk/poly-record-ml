module Lld = Lambda_let_dot
module Llp = Lambda_let_paren

(* IdxSet(tv::k) *)
let idxset_kind tv = function
  | Lld.KUniv -> []
  | Lld.KRecord xs ->
      List.map (fun (l, _) -> (l, Llp.TVar tv)) xs

(* IdxSet(F) where k = {{F}}*)
let idxset xs =
  let rec inner = function
  | [] -> []
  | (tv, k) :: tl -> idxset_kind tv k :: inner tl
  in inner xs |> List.flatten

(* (\tau)^* = \tau *)
let rec monotycon = function
  | Lld.TVar tv -> Llp.TVar tv
  | Lld.TInt -> Llp.TInt
  | Lld.TBool -> Llp.TBool
  | Lld.TFun (t1, t2) -> Llp.TFun (monotycon t1, monotycon t2)
  | Lld.TRecord ts ->
      let ts' =
        ts
        |> List.map (fun (l, t) -> (l, monotycon t))
        |> List.sort compare
      in
      Llp.TRecord ts'

let kcon = function
  | Lld.KUniv -> Llp.KUniv
  | Lld.KRecord xs ->
      let xs' =
        xs
        |> List.map (fun (l, t) -> (l, monotycon t))
      in
      Llp.KRecord xs'

(* (forall t_1::k_1. \cdots forall t_n::k_n.\tau)^*
 * = forall t_1::k_1. \cdots forall t_n::k_n.
 *   idx(l_1, t^'_1) => ... => idx(l_n, t^'_n) => \tau
 * *)
let rec tycon (Lld.Forall (xs, t)) =
  let idxsets = idxset xs in
  let xs' =
    xs
    |> List.map (fun (tv, k) -> (tv, kcon k))
  in
  match idxsets with
  | [] -> Llp.Forall (xs', monotycon t)
  | _ -> Llp.Forall (xs', Llp.TIdxFun (idxsets, monotycon t))

(* counter for fresh index variables *)
let __counter = ref 1

let reset_counter () =
  __counter := 1

let fresh_idxvar () =
  let rval = !__counter in
  __counter := !__counter + 1;
  rval

let rec compile (lbenv : Llp.lbenv) tyenv = function
  | Lld.EPolyInst (x, xs) ->
      let (Llp.Forall (ys, t)) = Environment.lookup x tyenv in
      begin match t with
        | Llp.TIdxFun (zs, _) ->
            let subs =
              List.map2 (fun (tv, _) ty -> (tv, monotycon ty)) ys xs
            in
            let idxs = List.map (fun (l, t) ->
              let t' = Llp.substitute subs t in
              try Llp.INat (Llp.idx_value l t')
              with Llp.Undefined_index_value ->
                Environment.lookup (l, t') lbenv
            ) zs
            in
            idxs
            |> List.fold_left (fun e idx -> Llp.EIdxApp (e, idx)) (Llp.EVar x)
        | _ ->
            (* Since there is no polymorphic instantiation
             * just convert to a variable expression
             * *)
            Llp.EVar x
      end
  | Lld.EInt i -> Llp.EInt i
  | Lld.EBool b -> Llp.EBool b
  | Lld.EBinOp (op, e1, e2) ->
      let e1' = compile lbenv tyenv e1 in
      let e2' = compile lbenv tyenv e2 in
      Llp.EBinOp (op, e1', e2')
  | Lld.EAbs (x, t, e) ->
      let t' = Llp.Forall ([], monotycon t) in
      let tyenv' = Environment.extend x t' tyenv in
      let e' = compile lbenv tyenv' e in
      Llp.EAbs (x, e')
  | Lld.EApp (e1, e2) ->
      let e1' = compile lbenv tyenv e1 in
      let e2' = compile lbenv tyenv e2 in
      Llp.EApp (e1', e2')
  | Lld.EPolyGen (e, (Lld.Forall (xs, _))) ->
      let xs' = idxset xs in
      let label_tv_idxvar_tuple_list =
        List.map (fun (l, t) -> (l, t, fresh_idxvar ())) xs'
      in
      let lbenv' =
        List.fold_left
        (fun env (l, t, i) -> Environment.extend (l, t) (Llp.IVar i) env)
        lbenv label_tv_idxvar_tuple_list
      in
      let e' = compile lbenv' tyenv e in
      let fresh_idxvars =
        List.rev_map (fun (_, _, i) -> i) label_tv_idxvar_tuple_list
      in
      fresh_idxvars
      |> List.fold_left (fun e idxv -> Llp.EIdxAbs (idxv, e)) e'
  | Lld.ELet (x, pt, e1, e2) ->
      let pt' = tycon pt in
      let e1' = compile lbenv tyenv e1 in
      let tyenv' = Environment.extend x pt' tyenv in
      let e2' = compile lbenv tyenv' e2 in
      Llp.ELet (x, e1', e2')
  | Lld.ERecord xs ->
      let xs' =
        xs
        |> List.sort compare
        |> List.map (fun (_, e) -> compile lbenv tyenv e)
      in
      Llp.EArray xs'
  | Lld.ERecordGet (e1, t, l) ->
      let e1' = compile lbenv tyenv e1 in
      let t' = monotycon t in
      let idx =
        try Llp.INat (Llp.idx_value l t')
        with Llp.Undefined_index_value ->
          Environment.lookup (l, t') lbenv
      in Llp.EArrayGet (e1', idx)
  | Lld.ERecordModify (e1, t, l, e2) ->
      let e1' = compile lbenv tyenv e1 in
      let t' = monotycon t in
      let idx =
        try Llp.INat (Llp.idx_value l t')
        with Llp.Undefined_index_value ->
          Environment.lookup (l, t') lbenv
      in
      let e2' = compile lbenv tyenv e2 in
      Llp.EArrayModify (e1', idx, e2')

(* entrypoint *)
let start exp =
  reset_counter ();
  compile Environment.empty Environment.empty exp
