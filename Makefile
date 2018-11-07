PACKS = oUnit
TESTS = \
				compiler_test.ml \
				evaluator_test.ml \
				typechecker_test.ml \
				test.ml
SOURCES = \
					syntax.ml \
					environment.ml environment.mli \
					evaluator.ml \
					lambda_let_paren.ml \
					lambda_let_dot.ml \
					compiler.ml \
					typechecker.ml \
					$(TESTS)
RESULT = test

test: bc
	./$(RESULT); rm ./$(RESULT)

-include OCamlMakefile
