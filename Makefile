# Make is being used mostly as a task runner that calls ocamlbuild.

.PHONY: all clean byte native profile debug sanity

OCB_FLAGS = -use-ocamlfind -I src -I lib
OCB = ocamlbuild $(OCB_FLAGS)

PKGS = core_kernel
PROBLEMS_ML = $(wildcard src/problem*.ml)
PROBLEMS_NATIVE = $(patsubst src/%.ml,%.native,$(PROBLEMS_ML))
PROBLEMS_BYTE = $(patsubst src/%.ml,%.byte,$(PROBLEMS_ML))

all: native byte

clean:
	$(OCB) -clean

native: sanity
	$(OCB) $(PROBLEMS_NATIVE)

byte: sanity
	$(OCB) $(PROBLEMS_BYTE)

profile: sanity
	$(OCB) -tag profile $(PROBLEMS_NATIVE)

debug: sanity
	$(OCB) -tag debug $(PROBLEMS_BYTE)

sanity:
	ocamlfind query $(PKGS)
