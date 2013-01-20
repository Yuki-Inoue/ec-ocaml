

.PHONY: all

OBJS = AIGame.cmo morpoin.cmo nestedmc.cmo playMorpoin.cmo

playMorpoin: $(OBJS)
	ocamlc -o $@ $(OBJS)

all: morpoin.cmo nestedmc.cmo playMorpoin.cmo


%.cmi : %.mli
	ocamlc -c $<

%.cmo : %.ml
	ocamlc -c $<

clean:
	rm $(wildcard *.cmo) $(wildcard *.cmi)


include .depend
