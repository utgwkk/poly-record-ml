.PHONY: all clean byte native test test_entrypoint

OCB_FLAGS = -use-ocamlfind -I src -I src/frontend
OCB = ocamlbuild $(OCB_FLAGS)

all: frontend

frontend: native

native:
	$(OCB) main.native

byte:
	$(OCB) main.byte

clean:
	$(OCB) -clean

test: test_entrypoint
	./entrypoint.native

test_entrypoint: OCB_FLAGS = -use-ocamlfind -I src -I src/frontend -I test
test_entrypoint:
	$(OCB) entrypoint.native
