open OUnit2
open Typechecker
open Syntax
open PolyRecord
open ExplicitlyTyped

let tests = "Typechecker_test">:::[
  "partial_check">:::[
    "x:forall X::U Y::U, X->Y->X |- x[int,bool] : int->bool->int">::(fun ctxt ->
      let kenv = Environment.empty in
      let tyenv =
        [
          ("x",
           Forall ([(1, KUniv); (2, KUniv)], TFun (TVar 1, TFun (TVar 2, TVar 1)))
          );
        ] |> Environment.from_list in
      let input = EPolyInst ("x", [TInt; TBool]) in
      let expected = Forall ([], TFun (TInt, TFun (TBool, TInt))) in
      assert_equal expected (type_check kenv tyenv input)
    );
  ];
  "entrypoint">:::[
    "success">:::[
      "|- 0 : int">::(fun ctxt ->
        let input = EInt 0 in
        let expected = Forall ([], TInt) in
        assert_equal expected (start input)
      );
      "|- 1 : int">::(fun ctxt ->
        let input = EInt 1 in
        let expected = Forall ([], TInt) in
        assert_equal expected (start input)
      );
      "|- true : bool">::(fun ctxt ->
        let input = EBool true in
        let expected = Forall ([], TBool) in
        assert_equal expected (start input)
      );
      "|- false : bool">::(fun ctxt ->
        let input = EBool false in
        let expected = Forall ([], TBool) in
        assert_equal expected (start input)
      );
      "|- 1 + 2 : int">::(fun ctxt ->
        let input = EBinOp (Plus, EInt 1, EInt 2) in
        let expected = Forall ([], TInt) in
        assert_equal expected (start input)
      );
      "|- 1 * 2 : int">::(fun ctxt ->
        let input = EBinOp (Mult, EInt 1, EInt 2) in
        let expected = Forall ([], TInt) in
        assert_equal expected (start input)
      );
      "|- 1 < 2 : bool">::(fun ctxt ->
        let input = EBinOp (Lt, EInt 1, EInt 2) in
        let expected = Forall ([], TBool) in
        assert_equal expected (start input)
      );
      "|- if true then 2 else 3 : int">::(fun ctxt ->
        let input = EIfThenElse (EBool true, EInt 2, EInt 3) in
        let expected = Forall ([], TInt) in
        assert_equal expected (start input)
      );
      "|- if false then 2 else 3 : int">::(fun ctxt ->
        let input = EIfThenElse (EBool true, EInt 2, EInt 3) in
        let expected = Forall ([], TInt) in
        assert_equal expected (start input)
      );
      "|- if 1 < 2 then 2 else 3 : int">::(fun ctxt ->
        let input = EIfThenElse (EBinOp (Lt, EInt 1, EInt 2), EInt 2, EInt 3) in
        let expected = Forall ([], TInt) in
        assert_equal expected (start input)
      );
      "|- fun x:int -> 1 : int -> int">::(fun ctxt ->
        let input = EAbs ("x", TInt, EInt 1) in
        let expected = Forall ([], TFun (TInt, TInt)) in
        assert_equal expected (start input)
      );
      "|- (fun x:int -> 1) 2 : int">::(fun ctxt ->
        let input = EApp (EAbs ("x", TInt, EInt 1), EInt 2) in
        let expected = Forall ([], TInt) in
        assert_equal expected (start input)
      );
      "|- {c=1, b=2, a=3} : {a:int, b:int, c:int}">::(fun ctxt ->
        let input = ERecord [("c", EInt 1); ("b", EInt 2); ("a", EInt 3)] in
        let expected = Forall ([], TRecord [("a", TInt); ("b", TInt); ("c", TInt)]) in
        assert_equal expected (start input)
      );
      "|- {c=1, b=2, a=3}:{a:int, b:int, c:int}.a : int">::(fun ctxt ->
        let input = ERecordGet (
            ERecord [("c", EInt 1); ("b", EInt 2); ("a", EInt 3)],
            TRecord [("c", TInt); ("b", TInt); ("a", TInt)],
            "a"
          ) in
        let expected = Forall ([], TInt) in
        assert_equal expected (start input)
      );
      "|- modify({c=1, b=2, a=3}:{c:int, b:int: a:int}, a, 100) : {a:int, b:int, c:int}">::(fun ctxt ->
        let input = ERecordModify (
            ERecord [("c", EInt 1); ("b", EInt 2); ("a", EInt 3)],
            TRecord [("c", TInt); ("b", TInt); ("a", TInt)],
            "a",
            EInt 100
          ) in
        let expected = Forall ([], TRecord [("a", TInt); ("b", TInt); ("c", TInt)]) in
        assert_equal expected (start input)
      );
      "Poly(1, forall t1::U.t2::U.t3::U.int) : forall t1::U.t2::U.t3::U.int">::(fun ctxt ->
        let input = EPolyGen (
          EInt 1,
          Forall ([1, KUniv; 2, KUniv; 3, KUniv], TInt)
        ) in
        let expected = Forall ([(1, KUniv); (2, KUniv); (3, KUniv)], TInt) in
        assert_equal expected (start input)
      );
      "Poly((1, forall t1::{{a:int, b:int}}.int) : forall t1::{{a:int, b:int}}.int">::(fun ctxt ->
        let input = EPolyGen (
          EInt 1,
          Forall ([1, KRecord [("a", TInt); ("b", TInt)]], TInt)
        ) in
        let expected = Forall ([(1, KRecord [("a", TInt); ("b", TInt)])], TInt) in
        assert_equal expected (start input)
      );
      "Poly((1, forall t1::{{a:int, b:int}}.forall t2::{{a:int}}.int)">::(fun ctxt ->
        let input = EPolyGen (
          EInt 1,
          Forall ([
            1, KRecord [("a", TInt); ("b", TInt)];
            2, KRecord [("a", TInt)];
          ], TInt)
        ) in
        let expected =
          Forall ([
            1, KRecord [("a", TInt); ("b", TInt)];
            2, KRecord [("a", TInt)];
          ], TInt)
        in
        assert_equal expected (start input)
      );
      "let_and_polyinst_no_type_application">::(fun ctxt ->
        (* let (x:{a:int, b:int}) = {a=1, b=2}
         * in x
         * *)
        let input = ELet (
          "x",
          Forall ([], TRecord [("a", TInt); ("b", TInt)]),
          ERecord [("a", EInt 1); ("b", EInt 2)],
          EPolyInst ("x", [])
        ) in
        let expected = Forall ([], TRecord [("a", TInt); ("b", TInt)]) in
        assert_equal expected (start input);
      );
      "polymorphic_age_function">::(fun ctxt ->
        (* Poly(fun x:t2 -> x:t2.age, forall t1::U.t2::{{age:t1}}.t2->t1)
         * *)
        let input = EPolyGen (
          EAbs (
            "x", TVar 2,
            ERecordGet (EPolyInst ("x", []), TVar 2, "age")
          ),
          Forall ([(1, KUniv); (2, KRecord [("age", TVar 1)])], TFun (TVar 2, TVar 1))
        ) in
        let expected = Forall (
          [(1, KUniv); (2, KRecord [("age", TVar 1)])],
          TFun (TVar 2, TVar 1)
        ) in
        assert_equal expected (start input)
      );
      "polymorphic_age_function_application">::(fun ctxt ->
        (* let age:forall t1::U.t2::{{age:t1}}.t2->t1)
         * = Poly(fun x:t2 -> x:t2.age, forall t1::U.t2::{{age:t1}}.t2->t1)
         * in (age int {age:int, roomno:int}) {age=22, roomno=403}
         * *)
        let input =
          ELet (
            "age",
            Forall ([(1, KUniv); (2, KRecord [("age", TVar 1)])], TFun (TVar 2, TVar 1)),
            EPolyGen (
              EAbs (
                "x", TVar 2,
                ERecordGet (EPolyInst ("x", []), TVar 2, "age")
              ),
              Forall ([(1, KUniv); (2, KRecord [("age", TVar 1)])], TFun (TVar 2, TVar 1))
            ),
            EApp (
              EPolyInst ("age", [TInt; TRecord [("age", TInt); ("roomno", TInt)]]),
              ERecord [("age", EInt 22); ("roomno", EInt 403)]
            )
          )
        in
        let expected = Forall ([], TInt) in
        assert_equal expected (start input)
      );
      "polymorphic_next_age_function_application">::(fun ctxt ->
        (* let next_age:forall t1::{{age:int}}.t1->int)
         * = Poly(fun x:t1 -> x:t1.age + 1, forall t1::{{age:int}}.t1->int)
         * in (next_age {age:int, roomno:int}) {age=22, roomno=403}
         * *)
        let input =
          ELet (
            "next_age",
            Forall ([(1, KRecord [("age", TInt)])], TFun (TVar 1, TInt)),
            EPolyGen (
              EAbs (
                "x", TVar 1,
                EBinOp (Plus, ERecordGet (EPolyInst ("x", []), TVar 1, "age"), EInt 1)
              ),
              Forall ([(1, KRecord [("age", TInt)])], TFun (TVar 1, TInt))
            ),
            EApp (
              EPolyInst ("next_age", [TRecord [("age", TInt); ("roomno", TInt)]]),
              ERecord [("age", EInt 22); ("roomno", EInt 403)]
            )
          )
        in
        let expected = Forall ([], TInt) in
        assert_equal expected (start input)
      );
    ];
    "failure">:::[
      "if_condition_is_not_boolean">::(fun ctxt ->
        let input = EIfThenElse (EInt 1, EInt 2, EInt 3) in
        assert_raises Typecheck_failed (fun () -> start input)
      );
      "if_type_unmatched">::(fun ctxt ->
        let input = EIfThenElse (EBool true, EInt 2, EBool false) in
        assert_raises Typecheck_failed (fun () -> start input)
      );
      "non_function_application">::(fun ctxt ->
        let input = EApp (EInt 1, EInt 2) in
        assert_raises Typecheck_failed (fun () -> start input)
      );
      "self_application_function">::(fun ctxt ->
        (* Poly(fun x:t1 -> x x, forall t1::U.t1 -> t1) *)
        let input =
          EPolyGen (
            EAbs ("x", TVar 1, EApp (EPolyInst ("x", []), EPolyInst ("x", []))),
            Forall ([(1, KUniv)], TFun (TVar 1, TVar 1)
          )
        ) in
        assert_raises Typecheck_failed (fun () -> start input)
      );
      "record_modify_with_different_type">::(fun ctxt ->
        let input = ERecordModify (
          ERecord [("a", EInt 3)], TRecord [("a", TInt)],
          "a", EBool true
        ) in
        assert_raises Typecheck_failed (fun () -> start input)
      );
      "record_modify_field_not_found">::(fun ctxt ->
        let input = ERecordModify (
          ERecord [("a", EInt 3)], TRecord [("a", TInt)],
          "b", EInt 5
        ) in
        assert_raises Typecheck_failed (fun () -> start input)
      );
      "let_type_annotation_wrong">::(fun ctxt ->
        let input = ELet (
          "x", Forall ([], TFun (TInt, TBool)),
          EInt 1, EPolyInst ("x", [])
        ) in
        assert_raises Typecheck_failed (fun () -> start input)
      );
      "polygen_type_annotation_wrong">::(fun ctxt ->
        let input = EPolyGen (
          EInt 1, Forall ([], TFun (TInt, TBool))
        ) in
        assert_raises Typecheck_failed (fun () -> start input)
      );
      "let x:forall t1.t1 = 1 in x int is rejected">::(fun ctxt ->
        let input = ELet (
          "x", Forall ([1, KUniv], TVar 1),
          EInt 1, EPolyInst ("x", [TInt])
        ) in
        assert_raises Typecheck_failed (fun () -> start input)
      );
    ]
  ]
]
