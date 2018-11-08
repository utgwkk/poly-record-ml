open OUnit2
open Typechecker
open Lambda_let_dot

let tests = "Typechecker_test">:::[
  "partial_check">:::[
    "int">::(fun ctxt ->
      let kenv = Environment.empty in
      let tyenv = Environment.empty in
      let input = EInt 1 in
      let expected = Forall ([], TInt) in
      assert_equal expected (type_check kenv tyenv input)
    );
    "x:forall X::U Y::U, X->Y->X |- x[int,int] : int->int->int">::(fun ctxt ->
      let kenv = Environment.empty in
      let tyenv =
        [
          ("x",
           Forall ([(1, KUniv); (2, KUniv)], TFun (TVar 1, TFun (TVar 2, TVar 1)))
          );
        ] |> Environment.from_list in
      let input = EPolyInst ("x", [TInt; TInt]) in
      let expected = Forall ([], TFun (TInt, TFun (TInt, TInt))) in
      assert_equal expected (type_check kenv tyenv input)
    );
  ];
  "entrypoint">:::[
    "success">:::[
      "0 => 0">::(fun ctxt ->
        let input = EInt 0 in
        let expected = Forall ([], TInt) in
        assert_equal expected (start input)
      );
      "1 => 1">::(fun ctxt ->
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
      "1_abs">::(fun ctxt ->
        (* (fun x:int -> 1) *)
        let input = EAbs ("x", TInt, EInt 1) in
        let expected = Forall ([], TFun (TInt, TInt)) in
        assert_equal expected (start input)
      );
      "1_abs_app">::(fun ctxt ->
        (* (fun x:int -> 1) 2 *)
        let input = EApp (EAbs ("x", TInt, EInt 1), EInt 2) in
        let expected = Forall ([], TInt) in
        assert_equal expected (start input)
      );
      "record_construct">::(fun ctxt ->
        (* {c=1, b=2, a=3} *)
        let input = ERecord [("c", EInt 1); ("b", EInt 2); ("a", EInt 3)] in
        let expected = Forall ([], TRecord [("a", TInt); ("b", TInt); ("c", TInt)]) in
        assert_equal expected (start input)
      );
      "record_access">::(fun ctxt ->
        (* ({c=1, b=2, a=3}:{c:int, b:int: a:int}).a *)
        let input = ERecordGet (
            ERecord [("c", EInt 1); ("b", EInt 2); ("a", EInt 3)],
            TRecord [("c", TInt); ("b", TInt); ("a", TInt)],
            "a"
          ) in
        let expected = Forall ([], TInt) in
        assert_equal expected (start input)
      );
      "record_modification">::(fun ctxt ->
        (* modify({c=1, b=2, a=3}:{c:int, b:int: a:int}, a, 100) *)
        let input = ERecordModify (
            ERecord [("c", EInt 1); ("b", EInt 2); ("a", EInt 3)],
            TRecord [("c", TInt); ("b", TInt); ("a", TInt)],
            "a",
            EInt 100
          ) in
        let expected = Forall ([], TRecord [("a", TInt); ("b", TInt); ("c", TInt)]) in
        assert_equal expected (start input)
      );
      "polygen_nonsense">::(fun ctxt ->
        (* Poly(1, forall t1::U. forall t2::U. forall t3::U.int) *)
        let input = EPolyGen (
          EInt 1,
          Forall ([1, KUniv; 2, KUniv; 3, KUniv], TInt)
        ) in
        let expected = Forall ([(1, KUniv); (2, KUniv); (3, KUniv)], TInt) in
        assert_equal expected (start input)
      );
      "polygen_record">::(fun ctxt ->
        (* Poly((1, forall t1::{{a:int, b:int}}.int) *)
        let input = EPolyGen (
          EInt 1,
          Forall ([1, KRecord [("a", TInt); ("b", TInt)]], TInt)
        ) in
        let expected = Forall ([(1, KRecord [("a", TInt); ("b", TInt)])], TInt) in
        assert_equal expected (start input)
      );
      "polygen_record_nested">::(fun ctxt ->
        (* Poly((1, forall t1::{{a:int, b:int}}.forall t2::{{a:int}}.int) *)
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
      "let_and_polyinst">::(fun ctxt ->
        (* let (x:forall t1::{{a:int, b:int}}.t1) = {a=1, b=2}
         * in x ({a:int, b:int})
         * *)
        let input = ELet (
          "x",
          Forall ([
            1, KRecord [("a", TInt); ("b", TInt)];
          ], TVar 1),
          ERecord [("a", EInt 1); ("b", EInt 2)],
          EPolyInst ("x", [TRecord [("a", TInt); ("b", TInt)]])
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
    ];
  ]
]
