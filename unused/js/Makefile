all:
	ocamlfind ocamlc -g -package js_of_ocaml,js_of_ocaml-ppx -linkpkg -o aaa.bytes aaa.ml
	js_of_ocaml --pretty aaa.bytes