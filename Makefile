PACKS = oUnit
TESTS = \
				evaluator_test.ml \
				test.ml
SOURCES = \
					syntax.ml \
					environment.ml environment.mli \
					evaluator.ml \
					lambda_let_paren.ml \
					$(TESTS)
RESULT = test

test: bc
	./test

-include OCamlMakefile
