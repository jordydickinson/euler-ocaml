# Make is being used mostly as a task runner that calls ocamlbuild.

.PHONY: all clean byte native profile debug sanity

OCB_FLAGS = -use-ocamlfind -I src -I lib
OCB = ocamlbuild $(OCB_FLAGS)

PKGS = core_kernel ppx_jane

all: native byte

clean:
	$(OCB) -clean

native: sanity
	$(OCB) main.native

byte: sanity
	$(OCB) main.byte

profile: sanity
	$(OCB) -tag profile main.native

debug: sanity
	$(OCB) -tag debug main.byte

sanity:
	ocamlfind query $(PKGS)
