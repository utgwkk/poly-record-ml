open OUnit2
open PolyRecord
open Infer

let tests = "Infer_test">:::[
  "success">:::([
    (
      ELet ("age",
            EAbs ("x", ERecordGet (EVar "x", "age")),
            EVar "age"),
      "forall 't0, 't1::#{age : 't0}. 't1 -> 't0"
    );
    (
      ELet ("s",
            EAbs ("x", EAbs ("y", EAbs ("z",
                  EApp (EApp (EVar "x", EVar "z"), EApp (EVar "y", EVar "z"))))),
            ELet ("k",
                  EAbs ("x", EAbs ("y", EVar "x")),
                  ELet ("i",
                        EApp (EApp (EVar "s", EVar "k"), EVar "k"),
                        EVar "i"))),
      "forall 't0. 't0 -> 't0"
    );
    (
      EAbs ("b",
            EAbs ("f",
                  ELet ("g1",
                        EAbs ("x", EApp (EVar "x", EVar "f")),
                        ELet ("g2",
                              EAbs ("x", EApp (EVar "x", EVar "f")),
                              EAbs ("z",
                                    EIfThenElse (EVar "b", EApp (EApp (EVar "g1", EVar "z"), EVar "g2"), EApp (EApp (EVar "g2", EVar "z"), EVar "g1"))))))),
      "forall 't0, 't1, 't2. bool -> 't0 -> ('t0 -> (('t0 -> 't1) -> 't1) -> 't2) -> 't2"
    );
    (
      ELet ("f", EAbs ("x", EInt 3), EBinOp (Plus, EApp (EVar "f", EBool true), EApp (EVar "f", EInt 4))),
      "int"
    );
    (
      EAbs ("b",
            ELet ("f",
                  EAbs ("x", EVar "x"),
                  ELet ("g", EAbs ("y", EVar "y"),
                        EIfThenElse (EVar "b", EApp (EVar "f", EVar "g"), EApp (EVar "g", EVar "f"))))),
      "forall 't0. bool -> 't0 -> 't0"
    );
    (
      ELet ("r",
            ERecord [("f", ERef (EAbs ("x", EVar "x"))); ("g", EAbs ("x", EVar "x"))],
            EIfThenElse (EApp (EDeref (ERecordGet (EVar "r", "f")), EBool true),
                         EVar "r",
                         EVar "r")),
      "forall 't0. {f : (bool -> bool) ref, g : 't0 -> 't0}"
    );
    (
      ELet ("r",
            ERecord [("f", ERef (EAbs ("x", EVar "x"))); ("g", EAbs ("x", EVar "x"))],
            EIfThenElse (EApp (EDeref (ERecordGet (EVar "r", "f")), EBool true),
                         EStatement (EApp (ERecordGet (EVar "r", "g"), EInt 2), EVar "r"),
                         EVar "r")),
      "{f : (bool -> bool) ref, g : int -> int}"
    );
  ]|> List.map (fun (input, expected) ->
    expected>::(fun ctxt ->
      let (_, _, actual) = start input in
      let actual_ty = pp_polyty actual in
      assert_equal ~printer:(fun x -> x) expected actual_ty
    )
  ));
  "failure">:::([
    (
      EAbs ("x",
            EApp (EVar "x", EVar "x")),
      "self application",
      "free variable 1 occurs inside TFun (TVar 1, TVar 2)"
    );
    (
      ELet ("rf",
            ERef (EAbs ("x", EVar "x")),
            EStatement (EBinOp (Assign,
                                EVar "rf",
                                EAbs ("x", EBinOp (Plus, EVar "x", EInt 1))),
                        EApp (EDeref (EVar "rf"), EBool true))),
      "invalid assignment",
      "type mismatch (TInt, TBool)"
    );
  ]|> List.map (fun (input, desc, error_msg) ->
    desc>::(fun ctxt ->
      assert_raises (Unify.Unification_failed error_msg) (fun () -> start input)
    )
  ))
]
