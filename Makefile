OCBFLAGS := -classic-display -use-menhir -j 0
OCB := ocamlbuild $(OCBFLAGS)

.PHONY: all debug clean top
all: main.native
debug: all main.cma

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
