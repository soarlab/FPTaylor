LIBS = unix,str,nums,interval
LIB_DIRS = /home/alexey/Work/Projects/OCaml/INTERVAL

all:
	ocamlbuild -cflags '-I $(LIB_DIRS)' -lflags '-I $(LIB_DIRS) $(LIB_DIRS)/libinterval.a' -libs $(LIBS) fptaylor.byte fptaylor.native
	mv fptaylor.byte fptaylor

clean:
	rm -rf _build
	-rm fptaylor fptaylor.native
	-rm *~ *.cmo *.cmi