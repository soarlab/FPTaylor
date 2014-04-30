LIBS = unix,str,nums,interval
LIB_DIRS = /home/alexey/Work/Projects/OCaml/INTERVAL

TEST_DIR = ../FPTaylor-old/tests
TESTS = test01_sum3.txt\
	test02_sum8.txt\
	test03_nonlin2.txt\
	test05_nonlin1.txt\
	test06_sums4.txt\
	test04_dqmom9.txt\

TEST = $(addprefix $(TEST_DIR)/,$(TESTS))

all:
	ocamlbuild -cflags '-I $(LIB_DIRS)' -lflags '-I $(LIB_DIRS) $(LIB_DIRS)/libinterval.a' -libs $(LIBS) fptaylor.byte #fptaylor.native
	mv fptaylor.byte fptaylor

test:
	./fptaylor $(TEST)

check1:
	./fptaylor -c test.cfg $(TEST_DIR)/test01_sum3.txt

check2:
	./fptaylor $(TEST_DIR)/test08_dqmom3.txt

jet:
	./fptaylor $(TEST_DIR)/darulova/jet.txt


clean:
	rm -rf _build
	rm -f fptaylor fptaylor.native
	rm -f *~ *.cmo *.cmi