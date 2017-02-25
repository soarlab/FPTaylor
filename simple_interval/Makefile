ML = ocamlc
OPT_ML = ocamlopt

SRC=	interval2.mli \
	interval2.ml \
	fpu.ml \
	interval.ml

OBJ_BYTE0 = $(SRC:.ml=.cmo)
OBJ_BYTE = $(OBJ_BYTE0:.mli=.cmi)
OBJ_NATIVE = $(OBJ_BYTE:.cmo=.cmx)

.PHONY: fast-interval clean

all: fast-interval interval.cma interval.cmxa

interval.cma: $(OBJ_BYTE)
	$(ML) -a -o interval.cma $(OBJ_BYTE0)

interval.cmxa: $(OBJ_NATIVE)
	$(OPT_ML) -a -o interval.cmxa $(OBJ_BYTE0:.cmo=.cmx)

fast-interval:
	cd fast_interval; $(MAKE)

%.cmi : %.mli
	$(ML) -c $^

%.cmo : %.ml
	$(ML) -c $^

%.cmx : %.ml
	$(OPT_ML) -c $^

clean:
	rm -f *.cmo *.cmi *.cmx *.cma *.cmxa *.o *.a
	cd fast_interval; $(MAKE) clean
