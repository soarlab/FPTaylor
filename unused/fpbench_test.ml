open Fpbench_parser
open Environment

let print_tokens str =
  let lexbuf = Lexing.from_string str in
  let end_flag = ref false in
  while not !end_flag do
    let token = Fpbench_lexer.token lexbuf in
    if token = EOF then
      end_flag := true
    else
      match token with
      | EOF -> Format.printf "EOF"
      | LPAREN -> Format.printf "("
      | RPAREN -> Format.printf ")"
      | LBRACKET -> Format.printf "["
      | RBRACKET -> Format.printf "]"
      | NUMBER str -> Format.printf "{Number %s}" str
      | SYMBOL str -> Format.printf "{Symbol %s}" str
      | STRING str -> Format.printf "{String %s}" str
  done

let parse_s_expr str =
  let lexbuf = Lexing.from_string str in
  let sexpr = parse_s_expr Fpbench_lexer.token lexbuf in
  print_s_expr sexpr;
  let data = translate_fpcore sexpr in
  Format.printf "\nname = %s\nprecision = %s\n"
    data.name (Rounding.rounding_to_string data.precision);
  List.iter Environment.print_raw_formula_std data.constraints;
  List.iter (fun (v, (Some lo, Some hi)) -> 
              Format.fprintf Format.std_formatter "%a <= %s <= %a\n" 
              Environment.print_raw_expr lo v Environment.print_raw_expr hi) data.vars;
  Environment.print_raw_expr_std data.expr

let process_file f fname =
  let lines = Lib.load_file fname in
  let str = String.concat "\n" lines in
  f str

let () = process_file print_tokens Sys.argv.(1)

let () = Format.print_newline (); Format.print_newline ()

(*let () = process_file parse_string Sys.argv.(1)*)

let () = process_file parse_s_expr Sys.argv.(1)

let () = Format.print_newline ()