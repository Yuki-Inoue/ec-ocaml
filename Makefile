

.PHONY: batches clean

batches: morpoin.cmo nestedmc.cmo
morpoin.cmo: AI.cmo


%.cmi : %.mli
	ocamlc -c $<
%.cmo : %.ml
	ocamlc -c $<

clean:
	rm $(wildcard *.cmo) $(wildcard *.cmi)
