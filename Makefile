ML = ocamlc
OPT_ML = ocamlopt
JSML = js_of_ocaml --opt 3
INTERVAL_DIR = INTERVAL
SIMPLE_INTERVAL_DIR = simple_interval
OPT_DIR = b_and_b

BASE_SRC = version.mli\
	version.ml\
	lib.mli\
	lib.ml\
	func.ml\
	log.mli\
	log.ml\
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
	config.mli\
	config.ml\
	input_parser_env.mli\
	input_parser_env.ml\
	input_parser.mli\
	input_parser.ml\
	input_lexer.ml\
	parser.mli\
	parser.ml

SRC = $(BASE_SRC)\
	proof_base.mli\
	proof_base.ml\
	proof.ml\
	rounding_simpl.mli\
	rounding_simpl.ml\
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

EXPORT_SRC=$(BASE_SRC)\
	out_fpcore.mli\
	out_fpcore.ml\
	export_main.ml

TEST_DIR=benchmarks/tests
TESTS= test01_sum3.txt\
	test02_sum8.txt\
	test03_nonlin2.txt\
	test05_nonlin1.txt\
	test06_sums4.txt

OBJ_BYTE0 = $(SRC:.ml=.cmo)
OBJ_BYTE = $(OBJ_BYTE0:.mli=.cmi)
OBJ_NATIVE = $(OBJ_BYTE:.cmo=.cmx)

OBJ_PROOF_SRC = $(PROOF_SRC:.ml=.cmo)
OBJ_EXPORT0 = $(EXPORT_SRC:.ml=.cmo)
OBJ_EXPORT = $(OBJ_EXPORT0:.mli=.cmi)

TEST = $(addprefix $(TEST_DIR)/,$(TESTS))

.PHONY: clean clean-tmp clean-interval clean-simple-interval compile-interval compile-simple-interval

all: fptaylor-interval

proof-tool: $(OBJ_PROOF_SRC)
	$(ML) -o proof_to_text unix.cma str.cma nums.cma $(OBJ_PROOF_SRC)

export-tool: INCLUDE=$(INTERVAL_DIR)

export-tool: $(OBJ_EXPORT)
	$(ML) -o export -I $(INTERVAL_DIR) \
		unix.cma str.cma nums.cma \
		$(INTERVAL_DIR)/chcw.o $(INTERVAL_DIR)/interval.cma \
		$(EXPORT_SRC:.ml=.cmo)

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
fptaylor-js-debug: ML=ocamlc -g
fptaylor-js-debug: JSML=js_of_ocaml --pretty

fptaylor-js: compile-simple-interval compile-byte default_config.js main_js.cmo
	ocamlfind $(ML) -o fptaylor.bytes -I $(OPT_DIR) -I $(SIMPLE_INTERVAL_DIR) \
		-package js_of_ocaml,js_of_ocaml-ppx,unix,str,num -linkpkg \
		$(SIMPLE_INTERVAL_DIR)/interval.cma \
		$(filter-out main.cmo, $(SRC:.ml=.cmo)) main_js.cmo
	$(JSML) -o fptaylor.js fptaylor.bytes

fptaylor-js-debug: fptaylor-js
	cp fptaylor.js js/
	cat default_config.js | sed -e 's/^export const/const/' > js/default_config.js

default_config.js: default.cfg
	echo "export const default_config = \`" > default_config.js
	cat default.cfg | sed -e 's/\`/\\\`/g; s/^opt[ ]*=.*/opt = bb-eval/; s/^log-base-dir[ ]*=.*/log-base-dir =/; s/^tmp-base-dir[ ]*=.*/tmp-base-dir =/' >> default_config.js
	echo "\`" >> default_config.js

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

main_js.cmo: main_js.ml
	ocamlfind ocamlc -c -I $(OPT_DIR) -I $(INCLUDE) \
		-package js_of_ocaml,js_of_ocaml-ppx \
		main_js.ml

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
	rm -f *~ *.o *.cmo *.cmi *.cmx *.pyc *.bytes
	cd $(OPT_DIR); $(MAKE) clean

clean-all: clean-interval clean-simple-interval clean

clean-tmp:
	find . -type f -name '*~' -delete
