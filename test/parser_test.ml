open OUnit2
open PolyRecord

let parse s = Parser.main Lexer.main (Lexing.from_string s)
let assert_parse expected s = assert_equal expected (parse s)

let tests = "Parser test">:::([
  (
    "1 + 2 * 3 < 3 * 4;;",
    EBinOp (Lt,
            EBinOp (Plus,
                    EInt 1,
                    EBinOp (Mult, EInt 2, EInt 3)),
            EBinOp (Mult, EInt 3, EInt 4))
  );
  (
    "f 1 + f 2;;",
    EBinOp (Plus,
            EApp (EVar "f", EInt 1),
            EApp (EVar "f", EInt 2))
  );
  (
    "fun x -> x;;",
    EAbs ("x", EVar "x")
  );
  (
    "{a = {b = 1}};;",
    ERecord ["a", ERecord ["b", EInt 1]];
  );
  (
    "let nextage = fun x -> x.age + 1
     in nextage {age = 22};;",
    ELet ("nextage",
          EAbs ("x",
                EBinOp (Plus, ERecordGet (EVar "x", "age"), EInt 1)),
          EApp (EVar "nextage", ERecord [("age", EInt 22)]))
  );
  ] |> List.map (fun (s, e) -> s >:: fun ctxt -> assert_parse e s)
)
