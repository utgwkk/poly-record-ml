PACKS = oUnit
OCAMLYACC = menhir
TESTS = \
				test/compiler_test.ml \
				test/evaluator_test.ml \
				test/typechecker_test.ml \
				test/entrypoint.ml
SOURCES = \
					syntax.ml \
					environment.ml environment.mli \
					evaluator.ml \
					explicitlyTyped.ml \
					implementation.ml \
					compiler.ml \
					typechecker.ml \
					parser.mly \
					lexer.mll
MAIN = main.ml
RESULT = prog

all: frontend

frontend: SOURCES += $(MAIN)
frontend: bc

.PHONY: test

test: SOURCES += $(TESTS)
test: bc
	./$(RESULT); rm $(RESULT)

include OCamlMakefile
