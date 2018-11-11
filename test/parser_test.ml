open OUnit2
open ExplicitlyTyped

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
            EApp (EPolyInst ("f", []), EInt 1),
            EApp (EPolyInst ("f", []), EInt 2))
  );
  (
    "f int 1 + f int 2;;",
    EBinOp (Plus,
            EApp (EPolyInst ("f", [TInt]), EInt 1),
            EApp (EPolyInst ("f", [TInt]), EInt 2))
  );
  (
    "let nextage:forall 't1::{{age:int}}.'t1->int =
     poly((fun (x:'t1) -> x:'t1.age + 1) : forall 't1::{{age:int}}.'t1->int)
     in (nextage {age:int}) {age = 22};;",
    ELet ("nextage",
          Forall ([1, KRecord ["age", TInt]], TFun (TVar 1, TInt)),
          EPolyGen (EAbs ("x", TVar 1,
                          EBinOp (Plus, ERecordGet (EPolyInst ("x", []), TVar 1, "age"), EInt 1)),
                    Forall ([1, KRecord ["age", TInt]], TFun (TVar 1, TInt))),
          EApp (EPolyInst ("nextage", [TRecord ["age", TInt]]), ERecord [("age", EInt 22)]))
  );
  ] |> List.map (fun (s, e) -> s >:: fun ctxt -> assert_parse e s)
)
