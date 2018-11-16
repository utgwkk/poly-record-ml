PACKS = oUnit
OCAMLYACC = menhir
SRCDIR = src
TESTS = \
				test/compiler_test.ml \
				test/evaluator_test.ml \
				test/typechecker_test.ml \
				test/parser_test.ml \
				test/infer_test.ml \
				test/entrypoint.ml
SOURCES = $(addprefix $(SRCDIR)/,\
					syntax.ml \
					environment.ml environment.mli \
					mySet.ml mySet.mli \
					evaluator.ml \
					polyRecord.ml \
					explicitlyTyped.ml \
					implementation.ml \
					infer.ml \
					compiler.ml \
					typechecker.ml \
					parser.mly \
					lexer.mll) \
					$(MAIN)
MAIN = $(SRCDIR)/main.ml
RESULT = prog

all: frontend

frontend: bc

.PHONY: test

test: MAIN = $(TESTS)
test: OCAMLFLAGS = -g
test: nc
	./$(RESULT); rm $(RESULT)

include OCamlMakefile
