OCBFLAGS := -classic-display -use-menhir -j 0
OCB := ocamlbuild $(OCBFLAGS)

.PHONY: all debug clean top
all: simulator_test.cma simulator_test.cmxa simulator_test.native
debug: all simulator_test.cma

%.cma:
	$(OCB) $@
%.cmxa:
	$(OCB) $@
%.native:
	$(OCB) $@
%.byte:
	$(OCB) $@

clean:
	$(OCB) -clean
	$(RM) src/version.ml*

top: debug
	ocaml
