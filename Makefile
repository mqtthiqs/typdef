
.PHONY: all clean

all: typdef.cmo test.cmi test.cmo

clean:
	rm -rf *.cm*

test.cmo: test.ml typdef.cmo
	ocamlc -pp "camlp5o -I . typdef.cmo" -c $<

%.cmo : %.ml
	ocamlc -pp camlp5o -I +camlp5 -c $<

%.cmi : %.mli
	ocamlc -pp camlp5o -I +camlp5 -c $<

