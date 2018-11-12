PACKS = oUnit
OCAMLYACC = menhir
TESTS = \
				test/compiler_test.ml \
				test/evaluator_test.ml \
				test/typechecker_test.ml \
				test/parser_test.ml \
				test/entrypoint.ml
SOURCES = \
					syntax.ml \
					environment.ml environment.mli \
					evaluator.ml \
					polyRecord.ml \
					explicitlyTyped.ml \
					implementation.ml \
					mySet.ml mySet.mli \
					infer.ml \
					compiler.ml \
					typechecker.ml \
					parser.mly \
					lexer.mll \
					$(MAIN)
MAIN = main.ml
RESULT = prog

all: frontend

frontend: bc

.PHONY: test

test: MAIN = $(TESTS)
test: bc
	./$(RESULT); rm $(RESULT)

include OCamlMakefile
