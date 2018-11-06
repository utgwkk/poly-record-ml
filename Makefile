PACKS = oUnit
TESTS = \
				evaluator_test.ml \
				test.ml
SOURCES = \
					$(TESTS)
RESULT = test

test: bc
	./test

-include OCamlMakefile
