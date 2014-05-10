LIBS = unix,str,nums,interval
INTERVAL_DIR = $(shell pwd)/INTERVAL

TEST_DIR = benchmarks/tests
TESTS = test01_sum3.txt\
	test02_sum8.txt\
	test03_nonlin2.txt\
	test05_nonlin1.txt\
	test06_sums4.txt


TEST = $(addprefix $(TEST_DIR)/,$(TESTS))

all:
	cd $(INTERVAL_DIR); $(MAKE)
	ocamlbuild -cflags '-I $(INTERVAL_DIR)' -lflags '-I $(INTERVAL_DIR) $(INTERVAL_DIR)/libinterval.a' -libs $(LIBS) fptaylor.byte #fptaylor.native
	mv fptaylor.byte fptaylor

test:
	./fptaylor $(TEST)

dqmom9:
	./fptaylor $(TEST_DIR)/test04_dqmom9.txt

check1:
	./fptaylor -c test.cfg $(TEST_DIR)/test01_sum3.txt

check2:
	./fptaylor $(TEST_DIR)/test08_dqmom3.txt

clean:
	rm -rf _build tmp log
	rm -f fptaylor fptaylor.native
	rm -f *~ *.cmo *.cmi *.pyc