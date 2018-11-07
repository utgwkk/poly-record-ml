open OUnit2
open Compiler

module Lld = Lambda_let_dot
module Llp = Lambda_let_paren

let tests = "Compiler_test">:::[
  "tycon_test">:::[
    "no_bound">::(fun ctxt ->
      let input = Lld.Forall ([], TInt) in
      let expected = Llp.Forall ([], TInt) in
      assert_equal expected (tycon input)
    );
    "universal">::(fun ctxt ->
      let input = Lld.Forall ([1, KUniv], TInt) in
      let expected = Llp.Forall ([1, KUniv], TInt) in
      assert_equal expected (tycon input)
    );
    "example_in_paper">::(fun ctxt ->
      (* forall t2 :: {{a:int, b:int}}.
       * forall t3 :: {{a:t2}}.
       * t2 -> t3
       * =>
       * forall t2 :: {{a:int, b:int}}.
       * forall t3 :: {{a:t2}}.
       * idx(a, t2) ==> idx(b, t2) ==> idx(a, t3) ==> t2 -> t3
       * *)
      let input =
        Lld.Forall ([
          (2, Lld.KRecord [("a", TInt); ("b", TInt)]);
          (3, Lld.KRecord [("a", TVar 2)])
        ], Lld.TFun (Lld.TVar 2, Lld.TVar 3))
      in
      let expected =
        Llp.Forall ([
          (2, Llp.KRecord [("a", TInt); ("b", TInt)]);
          (3, Llp.KRecord [("a", TVar 2)])
        ],
        Llp.TIdxFun ([
          ("a", TVar 2); ("b", TVar 2); ("a", TVar 3)
        ], Llp.TFun (TVar 2, TVar 3))
        )
      in
      assert_equal expected (tycon input)
    );
  ];
  "compile_test">:::[
    "0 => 0">::(fun ctxt ->
      let input = Lld.EInt 0 in
      let expected = Llp.EInt 0 in
      assert_equal expected (start input)
    );
    "1 => 1">::(fun ctxt ->
      let input = Lld.EInt 1 in
      let expected = Llp.EInt 1 in
      assert_equal expected (start input)
    );
    "1_abs">::(fun ctxt ->
      (* (fun x:int -> 1) => (fun x -> 1) *)
      let input = Lld.EAbs ("x", TInt, EInt 1) in
      let expected = Llp.EAbs ("x", EInt 1) in
      assert_equal expected (start input)
    );
    "1_abs_app">::(fun ctxt ->
      (* (fun x:int -> 1) 2 => (fun x -> 1) 2 *)
      let input = Lld.EApp (EAbs ("x", TInt, EInt 1), EInt 2) in
      let expected = Llp.EApp (EAbs ("x", EInt 1), EInt 2) in
      assert_equal expected (start input)
    );
    "record_construct">::(fun ctxt ->
      (* {c=1, b=2, a=3} => {3, 2, 1} *)
      let input = Lld.ERecord [("c", EInt 1); ("b", EInt 2); ("a", EInt 3)] in
      let expected = Llp.EArray [EInt 3; EInt 2; EInt 1] in
      assert_equal expected (start input)
    );
    "record_access">::(fun ctxt ->
      (* ({c=1, b=2, a=3}:{c:int, b:int: a:int}).a
       * => {3, 2, 1}[1] *)
      let input = Lld.ERecordGet (
          ERecord [("c", EInt 1); ("b", EInt 2); ("a", EInt 3)],
          TRecord [("c", TInt); ("b", TInt); ("a", TInt)],
          "a"
        ) in
      let expected = Llp.EArrayGet (
        Llp.EArray [EInt 3; EInt 2; EInt 1],
        INat 1
      ) in
      assert_equal expected (start input)
    );
    "record_modification">::(fun ctxt ->
      (* modify({c=1, b=2, a=3}:{c:int, b:int: a:int}, a, 100)
       * => modify({3, 2, 1}, 1, 100) *)
      let input = Lld.ERecordModify (
          ERecord [("c", EInt 1); ("b", EInt 2); ("a", EInt 3)],
          TRecord [("c", TInt); ("b", TInt); ("a", TInt)],
          "a",
          EInt 100
        ) in
      let expected = Llp.EArrayModify (
        Llp.EArray [EInt 3; EInt 2; EInt 1],
        INat 1,
        EInt 100
      ) in
      assert_equal expected (start input)
    );
    "polygen_nonsense">::(fun ctxt ->
      (* Poly(1, forall t1::U. forall t2::U. forall t3::U.int)
       * => 1
       * *)
      let input = Lld.EPolyGen (
        Lld.EInt 1,
        Lld.Forall ([1, KUniv; 2, KUniv; 3, KUniv], TInt)
      ) in
      let expected = Llp.EInt 1 in
      assert_equal expected (start input)
    );
    "polygen_record">::(fun ctxt ->
      (* Poly((1, forall t1::{{a:int, b:int}}.int)
       * => ifun i1 -> ifun i2 -> 1
       * *)
      let input = Lld.EPolyGen (
        Lld.EInt 1,
        Lld.Forall ([1, KRecord [("a", TInt); ("b", TInt)]], TInt)
      ) in
      let expected = Llp.EIdxAbs (1, Llp.EIdxAbs (2, Llp.EInt 1)) in
      assert_equal expected (start input)
    );
    "polygen_record_nested">::(fun ctxt ->
      (* Poly((1, forall t1::{{a:int, b:int}}.forall t2::{{a:int}}.int)
       * => ifun i1 -> ifun i2 -> ifun i3 -> 1
       * *)
      let input = Lld.EPolyGen (
        Lld.EInt 1,
        Lld.Forall ([
          1, KRecord [("a", TInt); ("b", TInt)];
          2, KRecord [("a", TInt)];
        ], TInt)
      ) in
      let expected = Llp.EIdxAbs (1, Llp.EIdxAbs (2, Llp.EIdxAbs (3, Llp.EInt 1))) in
      assert_equal expected (start input)
    );
    "let_and_polyinst">::(fun ctxt ->
      (* let (x:forall t1::{{a:int, b:int}}.t1) = {a=1, b=2}
       * in x ({a:int, b:int})
       * => x 1 2
       * *)
      let input = Lld.ELet (
        "x",
        Lld.Forall ([
          1, KRecord [("a", TInt); ("b", TInt)];
        ], TVar 1),
        Lld.ERecord [("a", EInt 1); ("b", EInt 2)],
        Lld.EPolyInst ("x", [TRecord [("a", TInt); ("b", TInt)]])
      ) in
      let expected = Llp.ELet (
        "x",
        EArray [EInt 1; EInt 2],
        Llp.EIdxApp (Llp.EIdxApp (EVar "x", INat 1), INat 2))
      in
      assert_equal expected (start input)
    );
    "polymorphic_age_function">::(fun ctxt ->
      (* Poly(fun x:t2 -> x:t2.age, forall t1::U.t2::{{name:t1}}.t2->t1)
       * => ifun i1 -> fun x -> x[i1]
       * *)
      let input = Lld.EPolyGen (
        EAbs (
          "x", TVar 2,
          ERecordGet (EPolyInst ("x", []), TVar 2, "age")
        ),
        Forall ([(1, KUniv); (2, KRecord [("age", TInt)])], TFun (TVar 2, TVar 1))
      ) in
      let expected = Llp.EIdxAbs (
        1,
        EAbs ("x", EArrayGet (EVar "x", IVar 1))
      )
      in
      assert_equal expected (start input)
    );
    "polymorphic_age_function_application">::(fun ctxt ->
      (* let age:forall t1::U.t2::{{name:t1}}.t2->t1)
       * = Poly(fun x:t2 -> x:t2.age, forall t1::U.t2::{{name:t1}}.t2->t1)
       * in (age int {age:int, roomno:int}) {age=22, roomno=403}
       * *)
      let input =
        Lld.ELet (
          "age",
          Forall ([(1, KUniv); (2, KRecord [("age", TVar 1)])], TFun (TVar 2, TVar 1)),
          Lld.EPolyGen (
            EAbs (
              "x", TVar 2,
              ERecordGet (EPolyInst ("x", []), TVar 2, "age")
            ),
            Forall ([(1, KUniv); (2, KRecord [("age", TVar 1)])], TFun (TVar 2, TVar 1))
          ),
          Lld.EApp (
            Lld.EPolyInst ("age", [TInt; TRecord [("age", TInt); ("roomno", TInt)]]),
            Lld.ERecord [("age", EInt 22); ("roomno", EInt 403)]
          )
        )
      in
      let expected = Llp.ELet ("age",
        Llp.EIdxAbs (
          1,
          EAbs ("x", EArrayGet (EVar "x", IVar 1))
        ),
        Llp.EApp (
          Llp.EIdxApp (EVar "age", INat 1),
          Llp.EArray [EInt 22; EInt 403]
        )
      )
      in
      assert_equal expected (start input)
    );
  ];
]
