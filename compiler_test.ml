open OUnit2
open Compiler

module Lld = Lambda_let_dot
module Llp = Lambda_let_paren

let tests = "Compiler_test">:::[
  "compile_test">:::[
    "compile_0_to_0">::(fun ctxt ->
      let input = Lld.EInt 0 in
      let expected = Llp.EInt 0 in
      assert_equal expected (start input)
    );
    "compile_1_to_1">::(fun ctxt ->
      let input = Lld.EInt 1 in
      let expected = Llp.EInt 1 in
      assert_equal expected (start input)
    );
    "compile_1_abs">::(fun ctxt ->
      (* (fun x:int -> 1) => (fun x -> 1) *)
      let input = Lld.EAbs ("x", TVar 1, EInt 1) in
      let expected = Llp.EAbs ("x", EInt 1) in
      assert_equal expected (start input)
    );
    "compile_1_abs_app">::(fun ctxt ->
      (* (fun x:int -> 1) 2 => (fun x -> 1) 2 *)
      let input = Lld.EApp (EAbs ("x", TVar 1, EInt 1), EInt 2) in
      let expected = Llp.EApp (EAbs ("x", EInt 1), EInt 2) in
      assert_equal expected (start input)
    );
    "compile_record">::(fun ctxt ->
      (* {c=1, b=2, a=3} => {3, 2, 1} *)
      let input = Lld.ERecord [("c", EInt 1); ("b", EInt 2); ("a", EInt 3)] in
      let expected = Llp.EArray [EInt 3; EInt 2; EInt 1] in
      assert_equal expected (start input)
    );
    "compile_record_access">::(fun ctxt ->
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
    "compile_record_modification">::(fun ctxt ->
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
  ];
]
