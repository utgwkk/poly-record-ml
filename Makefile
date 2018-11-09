PACKS = oUnit
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
					$(TESTS)
RESULT = prog

test: bc
	./$(RESULT); rm ./$(RESULT)

-include OCamlMakefile
