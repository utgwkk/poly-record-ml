open OUnit2
open Syntax
open Implementation

let tests = "Evaluator_test">:::[
  "zero_is_zero">::(fun ctxt ->
    (* 0 *)
    let exp = EInt 0 in
    let expected = VInt 0 in
    assert_equal expected (Evaluator.start exp)
  );
  "one_is_one">::(fun ctxt ->
		(* 1 *)
		let exp = EInt 1 in
		let expected = VInt 1 in
		assert_equal expected (Evaluator.start exp)
	);
  "3 + 2 = 5">::(fun ctxt ->
		let exp = EBinOp (Plus, EInt 3, EInt 2) in
		let expected = VInt 5 in
		assert_equal expected (Evaluator.start exp)
  );
  "3 * 2 = 6">::(fun ctxt ->
		let exp = EBinOp (Mult, EInt 3, EInt 2) in
		let expected = VInt 6 in
		assert_equal expected (Evaluator.start exp)
  );
  "1 < 2 = true">::(fun ctxt ->
		let exp = EBinOp (Lt, EInt 1, EInt 2) in
		let expected = VBool true in
		assert_equal expected (Evaluator.start exp)
  );
  "2 < 1 = false">::(fun ctxt ->
		let exp = EBinOp (Lt, EInt 2, EInt 1) in
    let expected = VBool false in
		assert_equal expected (Evaluator.start exp)
  );
  "true_is_true">::(fun ctxt ->
		let exp = EBool true in
		let expected = VBool true in
		assert_equal expected (Evaluator.start exp)
  );
  "false_is_false">::(fun ctxt ->
		let exp = EBool false in
		let expected = VBool false in
		assert_equal expected (Evaluator.start exp)
  );
  "if true then 2 else 3">::(fun ctxt ->
		let exp = EIfThenElse (EBool true, EInt 2, EInt 3) in
		let expected = VInt 2 in
		assert_equal expected (Evaluator.start exp)
  );
  "if false then 2 else 3">::(fun ctxt ->
		let exp = EIfThenElse (EBool false, EInt 2, EInt 3) in
		let expected = VInt 3 in
		assert_equal expected (Evaluator.start exp)
  );
  "id_abs">::(fun ctxt ->
		(* fun x -> x *)
		let exp = EAbs ("x", EVar "x") in
		let expected = VProc ("x", EVar "x", Environment.empty, Environment.empty) in
		assert_equal expected (Evaluator.start exp)
  );
  "id_abs_app">::(fun ctxt ->
		(* (fun x -> x) 2 *)
		let exp = EApp (EAbs ("x", EVar "x"), EInt 2) in
		let expected = VInt 2 in
		assert_equal expected (Evaluator.start exp)
  );
  "create_array">::(fun ctxt ->
		(* {3, 5, 7} *)
		let exp = EArray [EInt 3; EInt 5; EInt 7] in
		let expected = VArray [|VInt 3; VInt 5; VInt 7|] in
		assert_equal expected (Evaluator.start exp)
  );
  "access_array">::(fun ctxt ->
		(* {3, 5, 7}[2] *)
		let exp = EArrayGet (EArray [EInt 3; EInt 5; EInt 7], INat 2) in
		let expected = VInt 5 in
		assert_equal expected (Evaluator.start exp)
  );
  "modify_array">::(fun ctxt ->
		(* modify({3, 5, 7}, 2, 100) *)
		let exp = EArrayModify (EArray [EInt 3; EInt 5; EInt 7], INat 2, EInt 100) in
		let expected = VArray [|VInt 3; VInt 100; VInt 7|] in
		assert_equal expected (Evaluator.start exp)
  );
  "let_x_eq_2_in_x">::(fun ctxt ->
		(* let x = 2 in x *)
		let exp = ELet ("x", EInt 2, EVar "x") in
		let expected = VInt 2 in
		assert_equal expected (Evaluator.start exp)
  );
  "idxabs">::(fun ctxt ->
    (* ifun i1 -> 2 *)
		let exp = EIdxAbs (1, EInt 2) in
		let expected = VIdxAbs (1, EInt 2, Environment.empty) in
		assert_equal expected (Evaluator.start exp)
  );
  "idxabs_app_nonsense">::(fun ctxt ->
    (* (ifun i1 -> 2) 1 *)
		let exp = EIdxApp (EIdxAbs (1, EInt 2), INat 1) in
		let expected = VInt 2 in
		assert_equal expected (Evaluator.start exp)
  );
  "idxabs_app_array">::(fun ctxt ->
    (* (ifun i1 -> {3, 5, 7}[i1]) 2 *)
		let exp = EIdxApp (EIdxAbs (1, EArrayGet (EArray [EInt 3; EInt 5; EInt 7], IVar 1)), INat 2) in
		let expected = VInt 5 in
		assert_equal expected (Evaluator.start exp)
  );
  "nextage">::(fun ctxt ->
    let exp = ELet ("nextage",
                    EIdxAbs (1, EAbs ("x", EBinOp (Plus, EArrayGet (EVar "x", IVar 1), EInt 1))),
                    EApp (EIdxApp (EVar "nextage", INat 1), EArray [EInt 22; EInt 403])) in
    let expected = VInt 23 in
		assert_equal expected (Evaluator.start exp)
  );
  "(ifun i1 -> ((ifun i2 -> {1, 100, 1}) i2) i1) 2">::(fun ctxt ->
    let exp = EIdxApp (EIdxAbs (1, EIdxApp (EIdxAbs (2, EArrayGet (EArray [EInt 1; EInt 100; EInt 1], IVar 2)), IVar 1)), INat 2) in
    let expected = VInt 100 in
		assert_equal expected (Evaluator.start exp)
  );
]
