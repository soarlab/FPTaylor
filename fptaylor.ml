(* FPTaylor                                                                   *)
(* Alexey Solovyev, University of Utah                                        *)

open Lib
open List
open Parser

let exprs () = Environment.env.Environment.expressions

let process_input fname =
  let _ = report ("Loading: " ^ fname) in
  let _ = parse_file fname in
  let _ = map Expr.print_expr_std (exprs ()) in
  let _ = Format.pp_print_newline Format.std_formatter () in
  ()

let main () =
  let p = print_string in
  if Config.input_files = [] then
    let _ = p ("Usage: "
	       ^ Sys.argv.(0) 
	       ^ " [-c config1] [-c config2] ... input_file1 [input_file2 ...]\n") in
    exit 1
  else
    let _ = Config.print_options Format.std_formatter in
    let _ = map process_input Config.input_files in
    exit 0

let _ = main ()
