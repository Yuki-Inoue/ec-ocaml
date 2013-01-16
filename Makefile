

.PHONY: all

all: morpoin.cmo

morpoin.cmo : AIGame.cmo morpoin.cmi

%.cmi : %.mli
	ocamlc -c $<

%.cmo : %.ml
	ocamlc -c $<

clean:
	rm $(wildcard *.cmo) $(wildcard *.cmi)
