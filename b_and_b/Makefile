INTERVAL=../INTERVAL
INCLUDE=-I $(INTERVAL)
COMP=ocamlc -c $(INCLUDE)


EXT=$(INTERVAL)/libinterval.a\
    $(INTERVAL)/interval.cma


all: compile
	ocamlc $(INCLUDE) -o test\
		$(EXT)\
		pqueue.cmo b_and_b.cmo bb.cmo

compile: pqueue.ml b_and_b.ml main.ml opt0.ml
	$(COMP) pqueue.mli b_and_b.mli
	$(COMP) pqueue.ml b_and_b.ml bb.ml
	$(COMP) opt0.ml

clean:
	-rm -f *.cmi *.cmo *~ *.o *.cmx


