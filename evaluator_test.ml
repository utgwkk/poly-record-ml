open OUnit2
open Lambda_let_paren

let test_zero_is_zero ctxt =
  (* 0 *)
  let exp = EInt 0 in
  let expected = VInt 0 in
  assert_equal expected (Evaluator.start exp)

let test_one_is_one ctxt =
  (* 1 *)
  let exp = EInt 1 in
  let expected = VInt 1 in
  assert_equal expected (Evaluator.start exp)

let test_id_abs ctxt =
  (* fun x -> x *)
  let exp = EAbs ("x", EVar "x") in
  let expected = VProc ("x", EVar "x", Environment.empty) in
  assert_equal expected (Evaluator.start exp)

let test_id_abs_app ctxt =
  (* (fun x -> x) 2 *)
  let exp = EApp (EAbs ("x", EVar "x"), EInt 2) in
  let expected = VInt 2 in
  assert_equal expected (Evaluator.start exp)

let test_create_array ctxt =
  (* {3, 5, 7} *)
  let exp = EArray [EInt 3; EInt 5; EInt 7] in
  let expected = VArray [|VInt 3; VInt 5; VInt 7|] in
  assert_equal expected (Evaluator.start exp)

let test_access_array ctxt =
  (* {3, 5, 7}[1] *)
  let exp = EArrayGet (EArray [EInt 3; EInt 5; EInt 7], INat 1) in
  let expected = VInt 5 in
  assert_equal expected (Evaluator.start exp)

let test_modify_array ctxt =
  (* modify({3, 5, 7}, 1, 100) *)
  let exp = EArrayModify (EArray [EInt 3; EInt 5; EInt 7], INat 1, EInt 100) in
  let expected = VArray [|VInt 3; VInt 100; VInt 7|] in
  assert_equal expected (Evaluator.start exp)

let test_let_x_eq_2_in_x ctxt =
  (* let x = 2 in x *)
  let exp = ELet ("x", EInt 2, EVar "x") in
  let expected = VInt 2 in
  assert_equal expected (Evaluator.start exp)

let tests = "Evaluator_test">:::[
  "zero_is_zero">::test_zero_is_zero ;
  "one_is_one">::test_one_is_one ;
  "id_abs">::test_id_abs ;
  "id_abs_app">::test_id_abs_app ;
  "create_array">::test_create_array ;
  "access_array">::test_access_array ;
  "modify_array">::test_modify_array ;
  "let_x_eq_2_in_x">::test_let_x_eq_2_in_x ;
]
