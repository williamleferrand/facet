#
# Mitosis
#

all: core syntax

core:
	ocamlbuild lib/facet.cmo

syntax:
	ocamlbuild lib/syntax/pa_facet.cmo 

test: syntax
	camlp4o _build/lib/syntax/pa_facet.cmo  examples/test.ml # -filter Camlp4AstLifter

full: all 
	ocamlbuild examples/test.byte 

install: 
	ocamlfind install facet META _build/lib/facet.cm* _build/lib/syntax/pa_facet.cm*

clean:
	find . |grep '~'|xargs rm -rf 
	ocamlbuild -clean