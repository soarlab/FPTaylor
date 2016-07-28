ML = ocamlc
OPT_ML = ocamlopt
INTERVAL_DIR = INTERVAL
INCLUDE=$(INTERVAL_DIR)

SRC=	lib.ml\
	func.ml\
	log.mli\
	log.ml\
	config.mli\
	config.ml\
	more_num.ml\
	rounding.ml\
	binary_float.ml\
	expr.ml\
	eval.ml\
	environment.ml\
	input_parser.mli\
	input_parser.ml\
	input_lexer.ml\
	parser.ml\
	proof_base.ml\
	proof.ml\
	maxima.ml\
	opt_common.ml\
	opt_basic_bb.ml\
	opt_gelpia.ml\
	opt_z3.ml\
	opt_nlopt.ml\
	opt.ml\
	out_racket.ml \
	out_test.ml \
	rounding_simpl.ml\
	taylor_form.ml\
	fptaylor.ml

PROOF_SRC= lib.ml\
	   proof_base.ml\
	   proof_to_text.ml

TEST_DIR=benchmarks/tests
TESTS=	test01_sum3.txt\
	test02_sum8.txt\
	test03_nonlin2.txt\
	test05_nonlin1.txt\
	test06_sums4.txt

OBJ_BYTE0 = $(SRC:.ml=.cmo)
OBJ_BYTE = $(OBJ_BYTE0:.mli=.cmi)

OBJ_NATIVE = $(OBJ_BYTE:.cmo=.cmx)

OBJ_PROOF_SRC = $(PROOF_SRC:.ml=.cmo)

TEST = $(addprefix $(TEST_DIR)/,$(TESTS))

all: compile-interval compile-byte
	$(ML) -o fptaylor -I $(INCLUDE) \
		unix.cma str.cma nums.cma \
		$(INTERVAL_DIR)/chcw.o interval.cma \
		$(SRC:.ml=.cmo)

proof-tool: $(OBJ_PROOF_SRC)
	$(ML) -o proof_to_text unix.cma str.cma nums.cma $(OBJ_PROOF_SRC)

tests:
	./fptaylor $(TEST)

compile-interval:
	cd $(INTERVAL_DIR); $(MAKE)

compile-byte: $(OBJ_BYTE)
	@echo "FPTaylor compiled (bytecode)"

compile-native: $(OBJ_NATIVE)
	@echo "FPTaylor compiled (native)"

input_lexer.ml: input_lexer.mll
	ocamllex input_lexer.mll

input_parser.ml input_parser.mli: input_parser.mly
	ocamlyacc input_parser.mly

%.cmi : %.mli
	$(ML) -c -I $(INCLUDE) $^

%.cmo : %.ml
	$(ML) -c -I $(INCLUDE) $^

%.cmx : %.ml
	$(OPT_ML) -c -I $(INCLUDE) $^

clean-interval:
	cd $(INTERVAL_DIR); $(MAKE) clean

clean:
	rm -rf _build tmp log
	rm -f fptaylor fptaylor.native
	rm -f input_parser.ml input_parser.mli input_lexer.ml input_lexer.mli
	rm -f *~ *.o *.cmo *.cmi *.cmx *.pyc

clean-all: clean-interval clean

clean-tmp:
	find . -type f -name '*~' -delete
