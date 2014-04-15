all:
	ocamlbuild -libs unix,str,nums fptaylor.byte
	mv fptaylor.byte fptaylor

clean:
	rm -rf _build
	-rm fptaylor
	-rm *~ *.cmo *.cmi