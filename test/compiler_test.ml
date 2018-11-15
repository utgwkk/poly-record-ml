open OUnit2
open Compiler
open Syntax

module PL = PolyRecord
module ET = ExplicitlyTyped
module Impl = Implementation

let start = start Environment.empty

let tests = "Compiler_test">:::[
  "tycon_test">:::[
    "no_bound">::(fun ctxt ->
      let input = PL.Forall ([], TInt) in
      let expected = Impl.Forall ([], TInt) in
      assert_equal expected (tycon input)
    );
    "universal">::(fun ctxt ->
      let input = PL.Forall ([1, KUniv], TInt) in
      let expected = Impl.Forall ([1, KUniv], TInt) in
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
        PL.Forall ([
          (2, PL.KRecord [("a", TInt); ("b", TInt)]);
          (3, PL.KRecord [("a", TVar 2)])
        ], PL.TFun (PL.TVar 2, PL.TVar 3))
      in
      let expected =
        Impl.Forall ([
          (2, Impl.KRecord [("a", TInt); ("b", TInt)]);
          (3, Impl.KRecord [("a", TVar 2)])
        ],
        Impl.TIdxFun ([
          ("a", TVar 2); ("b", TVar 2); ("a", TVar 3)
        ], Impl.TFun (TVar 2, TVar 3))
        )
      in
      assert_equal expected (tycon input)
    );
  ];
  "compile_test">:::[
    "0 => 0">::(fun ctxt ->
      let input = ET.EInt 0 in
      let expected = Impl.EInt 0 in
      assert_equal expected (start input)
    );
    "1 => 1">::(fun ctxt ->
      let input = ET.EInt 1 in
      let expected = Impl.EInt 1 in
      assert_equal expected (start input)
    );
    "true => true">::(fun ctxt ->
      let input = ET.EBool true in
      let expected = Impl.EBool true in
      assert_equal expected (start input)
    );
    "false => false">::(fun ctxt ->
      let input = ET.EBool false in
      let expected = Impl.EBool false in
      assert_equal expected (start input)
    );
    "1 + 2">::(fun ctxt ->
      let input = ET.EBinOp (Plus, EInt 1, EInt 2) in
      let expected = Impl.EBinOp (Plus, EInt 1, EInt 2) in
      assert_equal expected (start input)
    );
    "1 * 2">::(fun ctxt ->
      let input = ET.EBinOp (Mult, EInt 1, EInt 2) in
      let expected = Impl.EBinOp (Mult, EInt 1, EInt 2) in
      assert_equal expected (start input)
    );
    "1 < 2">::(fun ctxt ->
      let input = ET.EBinOp (Lt, EInt 1, EInt 2) in
      let expected = Impl.EBinOp (Lt, EInt 1, EInt 2) in
      assert_equal expected (start input)
    );
    "if true then 2 else 3">::(fun ctxt ->
      let input = ET.EIfThenElse (EBool true, EInt 2, EInt 3) in
      let expected = Impl.EIfThenElse (EBool true, EInt 2, EInt 3) in
      assert_equal expected (start input)
    );
    "1_abs">::(fun ctxt ->
      (* (fun x:int -> 1) => (fun x -> 1) *)
      let input = ET.EAbs ("x", TInt, EInt 1) in
      let expected = Impl.EAbs ("x", EInt 1) in
      assert_equal expected (start input)
    );
    "1_abs_app">::(fun ctxt ->
      (* (fun x:int -> 1) 2 => (fun x -> 1) 2 *)
      let input = ET.EApp (EAbs ("x", TInt, EInt 1), EInt 2) in
      let expected = Impl.EApp (EAbs ("x", EInt 1), EInt 2) in
      assert_equal expected (start input)
    );
    "record_construct">::(fun ctxt ->
      (* {c=1, b=2, a=3} => {3, 2, 1} *)
      let input = ET.ERecord [("c", EInt 1); ("b", EInt 2); ("a", EInt 3)] in
      let expected = Impl.EArray [EInt 3; EInt 2; EInt 1] in
      assert_equal expected (start input)
    );
    "record_access">::(fun ctxt ->
      (* ({c=1, b=2, a=3}:{c:int, b:int: a:int}).a
       * => {3, 2, 1}[1] *)
      let input = ET.ERecordGet (
          ERecord [("c", EInt 1); ("b", EInt 2); ("a", EInt 3)],
          TRecord [("c", TInt); ("b", TInt); ("a", TInt)],
          "a"
        ) in
      let expected = Impl.EArrayGet (
        Impl.EArray [EInt 3; EInt 2; EInt 1],
        INat 1
      ) in
      assert_equal expected (start input)
    );
    "record_modification">::(fun ctxt ->
      (* modify({c=1, b=2, a=3}:{c:int, b:int: a:int}, a, 100)
       * => modify({3, 2, 1}, 1, 100) *)
      let input = ET.ERecordModify (
          ERecord [("c", EInt 1); ("b", EInt 2); ("a", EInt 3)],
          TRecord [("c", TInt); ("b", TInt); ("a", TInt)],
          "a",
          EInt 100
        ) in
      let expected = Impl.EArrayModify (
        Impl.EArray [EInt 3; EInt 2; EInt 1],
        INat 1,
        EInt 100
      ) in
      assert_equal expected (start input)
    );
    "polygen_nonsense">::(fun ctxt ->
      (* Poly(1, forall t1::U. forall t2::U. forall t3::U.int)
       * => 1
       * *)
      let input = ET.EPolyGen (
        ET.EInt 1,
        PL.Forall ([1, KUniv; 2, KUniv; 3, KUniv], TInt)
      ) in
      let expected = Impl.EInt 1 in
      assert_equal expected (start input)
    );
    "polygen_record">::(fun ctxt ->
      (* Poly((1, forall t1::{{a:int, b:int}}.int)
       * => ifun i1 -> ifun i2 -> 1
       * *)
      let input = ET.EPolyGen (
        ET.EInt 1,
        PL.Forall ([1, KRecord [("a", TInt); ("b", TInt)]], TInt)
      ) in
      let expected = Impl.EIdxAbs (1, Impl.EIdxAbs (2, Impl.EInt 1)) in
      assert_equal expected (start input)
    );
    "polygen_record_nested">::(fun ctxt ->
      (* Poly((1, forall t1::{{a:int, b:int}}.forall t2::{{a:int}}.int)
       * => ifun i1 -> ifun i2 -> ifun i3 -> 1
       * *)
      let input = ET.EPolyGen (
        ET.EInt 1,
        PL.Forall ([
          1, KRecord [("a", TInt); ("b", TInt)];
          2, KRecord [("a", TInt)];
        ], TInt)
      ) in
      let expected = Impl.EIdxAbs (1, Impl.EIdxAbs (2, Impl.EIdxAbs (3, Impl.EInt 1))) in
      assert_equal expected (start input)
    );
    "let_and_polyinst">::(fun ctxt ->
      (* let (x:forall t1::{{a:int, b:int}}.t1) = {a=1, b=2}
       * in x ({a:int, b:int})
       * => x 1 2
       * *)
      let input = ET.ELet (
        "x",
        PL.Forall ([
          1, KRecord [("a", TInt); ("b", TInt)];
        ], TVar 1),
        ET.ERecord [("a", EInt 1); ("b", EInt 2)],
        ET.EPolyInst ("x", [TRecord [("a", TInt); ("b", TInt)]])
      ) in
      let expected = Impl.ELet (
        "x",
        EArray [EInt 1; EInt 2],
        Impl.EIdxApp (Impl.EIdxApp (EVar "x", INat 1), INat 2))
      in
      assert_equal expected (start input)
    );
    "polymorphic_age_function">::(fun ctxt ->
      (* Poly(fun x:t2 -> x:t2.age, forall t1::U.t2::{{name:t1}}.t2->t1)
       * => ifun i1 -> fun x -> x[i1]
       * *)
      let input = ET.EPolyGen (
        EAbs (
          "x", TVar 2,
          ERecordGet (EPolyInst ("x", []), TVar 2, "age")
        ),
        Forall ([(1, KUniv); (2, KRecord [("age", TInt)])], TFun (TVar 2, TVar 1))
      ) in
      let expected = Impl.EIdxAbs (
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
        ET.ELet (
          "age",
          Forall ([(1, KUniv); (2, KRecord [("age", TVar 1)])], TFun (TVar 2, TVar 1)),
          ET.EPolyGen (
            EAbs (
              "x", TVar 2,
              ERecordGet (EPolyInst ("x", []), TVar 2, "age")
            ),
            Forall ([(1, KUniv); (2, KRecord [("age", TVar 1)])], TFun (TVar 2, TVar 1))
          ),
          ET.EApp (
            ET.EPolyInst ("age", [TInt; TRecord [("age", TInt); ("roomno", TInt)]]),
            ET.ERecord [("age", EInt 22); ("roomno", EInt 403)]
          )
        )
      in
      let expected = Impl.ELet ("age",
        Impl.EIdxAbs (
          1,
          EAbs ("x", EArrayGet (EVar "x", IVar 1))
        ),
        Impl.EApp (
          Impl.EIdxApp (EVar "age", INat 1),
          Impl.EArray [EInt 22; EInt 403]
        )
      )
      in
      assert_equal expected (start input)
    );
  ];
]
