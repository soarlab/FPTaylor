ML = ocamlc
OPT_ML = ocamlopt
INTERVAL_DIR = INTERVAL
SIMPLE_INTERVAL_DIR = simple_interval
OPT_DIR = b_and_b

SRC=	version.mli\
	version.ml\
	lib.mli\
	lib.ml\
	func.ml\
	log.mli\
	log.ml\
	config.mli\
	config.ml\
	more_num.mli\
	more_num.ml\
	rounding.mli\
	rounding.ml\
	binary_float.ml\
	const.mli\
	const.ml\
	expr.mli\
	expr.ml\
	exprOut.mli\
	exprOut.ml\
	eval.mli\
	eval.ml\
	task.mli\
	task.ml\
	input_parser_env.mli\
	input_parser_env.ml\
	input_parser.mli\
	input_parser.ml\
	input_lexer.ml\
	parser.mli\
	parser.ml\
	proof_base.mli\
	proof_base.ml\
	proof.ml\
	maxima.mli\
	maxima.ml\
	opt_common.mli\
	opt_common.ml\
	opt_basic_bb.mli\
	opt_basic_bb.ml\
	opt_gelpia.mli\
	opt_gelpia.ml\
	opt_z3.mli\
	opt_z3.ml\
	opt_nlopt.mli\
	opt_nlopt.ml\
	$(OPT_DIR)/opt0.mli\
	$(OPT_DIR)/opt0.ml\
	opt_bb_eval.mli\
	opt_bb_eval.ml\
	opt.mli\
	opt.ml\
	out_test.ml\
	rounding_simpl.mli\
	rounding_simpl.ml\
	out_fpcore.mli\
	out_fpcore.ml\
	out_error_bounds.mli\
	out_error_bounds.ml\
	taylor_form.mli\
	taylor_form.ml\
	fptaylor.mli\
	fptaylor.ml\
	main.ml

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

.PHONY: clean clean-tmp clean-interval clean-simple-interval compile-interval compile-simple-interval

all: fptaylor-interval

proof-tool: $(OBJ_PROOF_SRC)
	$(ML) -o proof_to_text unix.cma str.cma nums.cma $(OBJ_PROOF_SRC)

test:
	./fptaylor $(TEST)

fptaylor-interval: INCLUDE=$(INTERVAL_DIR)

fptaylor-interval: compile-interval compile-byte
	$(ML) -o fptaylor -I $(OPT_DIR) -I $(INTERVAL_DIR) \
		unix.cma str.cma nums.cma \
		$(INTERVAL_DIR)/chcw.o $(INTERVAL_DIR)/interval.cma \
		$(SRC:.ml=.cmo)
	cp $(OPT_DIR)/compile.template $(OPT_DIR)/compile.sh
	echo "compile_native_with_INTERVAL" >> $(OPT_DIR)/compile.sh
	chmod +x $(OPT_DIR)/compile.sh	

fptaylor-simple-interval: INCLUDE=$(SIMPLE_INTERVAL_DIR)

fptaylor-simple-interval: compile-simple-interval compile-byte
	$(ML) -o fptaylor -I $(OPT_DIR) -I $(SIMPLE_INTERVAL_DIR) \
		unix.cma str.cma nums.cma \
		$(SIMPLE_INTERVAL_DIR)/interval.cma \
		$(SRC:.ml=.cmo)
	cp $(OPT_DIR)/compile.template $(OPT_DIR)/compile.sh
	echo "compile_native_with_simple_interval" >> $(OPT_DIR)/compile.sh
	chmod +x $(OPT_DIR)/compile.sh	

fptaylor-simple-interval2: INCLUDE=$(SIMPLE_INTERVAL_DIR)

fptaylor-simple-interval2: compile-simple-interval compile-byte
	$(ML) -o fptaylor -I $(OPT_DIR) -I $(SIMPLE_INTERVAL_DIR) \
		unix.cma str.cma nums.cma \
		$(SIMPLE_INTERVAL_DIR)/interval.cma \
		$(SRC:.ml=.cmo)
	cp $(OPT_DIR)/compile.template $(OPT_DIR)/compile.sh
	echo "compile_native_with_simple_interval2" >> $(OPT_DIR)/compile.sh
	chmod +x $(OPT_DIR)/compile.sh	

fptaylor-js: INCLUDE=$(SIMPLE_INTERVAL_DIR)

fptaylor-js: compile-simple-interval compile-byte
	ocamlfind ocamlc -o fptaylor.bytes -I $(OPT_DIR) -I $(SIMPLE_INTERVAL_DIR) \
		-package js_of_ocaml,js_of_ocaml-ppx,unix,str,num -linkpkg \
		$(SIMPLE_INTERVAL_DIR)/interval.cma \
		$(SRC:.ml=.cmo)
	js_of_ocaml --opt 3 fptaylor.bytes

compile-interval:
	cd $(INTERVAL_DIR); $(MAKE)

compile-simple-interval:
	cd $(SIMPLE_INTERVAL_DIR); $(MAKE)

compile-byte: $(OBJ_BYTE)
	@echo "FPTaylor compiled (bytecode)"

compile-native: $(OBJ_NATIVE)
	@echo "FPTaylor compiled (native)"

input_lexer.ml: input_lexer.mll
	ocamllex input_lexer.mll

input_parser.ml input_parser.mli: input_parser.mly
	ocamlyacc input_parser.mly

%.cmi : %.mli
	$(ML) -c -I $(OPT_DIR) -I $(INCLUDE) $^

%.cmo : %.ml
	$(ML) -c -I $(OPT_DIR) -I $(INCLUDE) $^

%.cmx : %.ml
	$(OPT_ML) -c -I $(OPT_DIR) -I $(INCLUDE) $^

clean-interval:
	cd $(INTERVAL_DIR); $(MAKE) clean

clean-simple-interval:
	cd $(SIMPLE_INTERVAL_DIR); $(MAKE) clean

clean:
	rm -rf _build tmp log
	rm -f fptaylor fptaylor.native
	rm -f input_parser.ml input_parser.mli input_lexer.ml input_lexer.mli
	rm -f *~ *.o *.cmo *.cmi *.cmx *.pyc
	cd $(OPT_DIR); $(MAKE) clean

clean-all: clean-interval clean-simple-interval clean

clean-tmp:
	find . -type f -name '*~' -delete
