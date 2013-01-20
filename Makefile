

.PHONY: all

OBJS = AIGame.cmo morpoin.cmo nestedmc.cmo playMorpoin.cmo
DEBUG = -g

playMorpoin: $(OBJS)
	ocamlc $(DEBUG) -o $@ $(OBJS)

all: morpoin.cmo nestedmc.cmo playMorpoin.cmo


%.cmi : %.mli
	ocamlc -c $(DEBUG) $<

%.cmo : %.ml
	ocamlc -c $(DEBUG) $<

clean:
	rm $(wildcard *.cmo) $(wildcard *.cmi)


include .depend
