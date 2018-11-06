open OUnit2
open Lambda_let_paren

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
  "id_abs">::(fun ctxt ->
		(* fun x -> x *)
		let exp = EAbs ("x", EVar "x") in
		let expected = VProc ("x", EVar "x", Environment.empty) in
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
		(* {3, 5, 7}[1] *)
		let exp = EArrayGet (EArray [EInt 3; EInt 5; EInt 7], INat 1) in
		let expected = VInt 5 in
		assert_equal expected (Evaluator.start exp)
  );
  "modify_array">::(fun ctxt ->
		(* modify({3, 5, 7}, 1, 100) *)
		let exp = EArrayModify (EArray [EInt 3; EInt 5; EInt 7], INat 1, EInt 100) in
		let expected = VArray [|VInt 3; VInt 100; VInt 7|] in
		assert_equal expected (Evaluator.start exp)
  );
  "let_x_eq_2_in_x">::(fun ctxt ->
		(* let x = 2 in x *)
		let exp = ELet ("x", EInt 2, EVar "x") in
		let expected = VInt 2 in
		assert_equal expected (Evaluator.start exp)
  );
]
