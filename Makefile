
.PHONY: all test clean

all: typdef.cmo test

test: test.cmi test.cmo

clean:
	rm -rf *.cm*

test.cmo: test.ml test.mli typdef.cmo
	ocamlc -pp "camlp5o -I . typdef.cmo" -c $<

%.cmo : %.ml
	ocamlc -pp camlp5o -I +camlp5 -c $<

%.cmi : %.mli
	ocamlc -pp camlp5o -I +camlp5 -c $<

