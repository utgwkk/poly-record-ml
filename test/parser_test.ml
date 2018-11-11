open OUnit2

let parse s = Parser.main Lexer.main (Lexing.from_string s)
let assert_parse expected s = assert_equal expected (parse s)

let tests = "Parser test">:::[
]
