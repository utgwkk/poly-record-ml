open OUnit2
open PolyRecord
open Infer

let tests = "Infer_test">:::([
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
  ]|> List.map (fun (input, expected) ->
    expected>::(fun ctxt ->
      let (_, _, actual) = start input in
      let actual_ty = pp_polyty actual in
      assert_equal ~printer:(fun x -> x) expected actual_ty
    )
  )
)
