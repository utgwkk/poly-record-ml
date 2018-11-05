SOURCES = syntax.ml parser.mly lexer.mll env.ml env.mli mySet.ml mySet.mli eval.ml main.ml
RESULT = prog

OCAMLYACC = menhir

all: nc

-include OCamlMakefile
