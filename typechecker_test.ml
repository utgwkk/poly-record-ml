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
]
