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
  ]|> List.map (fun (input, expected) ->
    expected>::(fun ctxt ->
      let (_, _, actual) = start input in
      let actual_ty = pp_polyty actual in
      assert_equal ~printer:(fun x -> x) expected actual_ty
    )
  )
)
