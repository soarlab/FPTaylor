ML = ocamlc
OPT_ML = ocamlopt

SRC=	interval1.mli \
	interval1.ml \
	fpu.ml \
	interval.ml

OBJ_BYTE0 = $(SRC:.ml=.cmo)
OBJ_BYTE = $(OBJ_BYTE0:.mli=.cmi)
OBJ_NATIVE = $(OBJ_BYTE:.cmo=.cmx)

.PHONY: clean

all: fast_interval.cma fast_interval.cmxa

fast_interval.cma: $(OBJ_BYTE)
	$(ML) -a -o fast_interval.cma $(OBJ_BYTE0)

fast_interval.cmxa: $(OBJ_NATIVE)
	$(OPT_ML) -a -o fast_interval.cmxa $(OBJ_BYTE0:.cmo=.cmx)

%.cmi : %.mli
	$(ML) -c $^

%.cmo : %.ml
	$(ML) -c $^

%.cmx : %.ml
	$(OPT_ML) -c $^

clean:
	rm -f *.cmo *.cmi *.cmx *.cma *.cmxa *.o *.a
