open OUnit2

let all_tests = [
  Evaluator_test.tests;
  Compiler_test.tests;
  Typechecker_test.tests;
  Parser_test.tests;
  Infer_test.tests;
]

let () = run_test_tt_main ("tests">:::all_tests)
