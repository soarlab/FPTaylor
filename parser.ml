(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT license           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* Auxiliary parser functions                                                 *)
(* -------------------------------------------------------------------------- *)

let parse_raw_expr str =
  let lexbuf = Lexing.from_string str in
  Input_parser.expr Input_lexer.token lexbuf

let parse_expr str =
  let raw = parse_raw_expr str in
  Input_parser_env.transform_raw_expr raw

let parse_var_type str =
  Input_parser.var_type Input_lexer.token (Lexing.from_string str)

let parse_rnd str =
  Input_parser.rnd Input_lexer.token (Lexing.from_string str)

let parse_string str =
  Input_parser_env.reset();
  let lexbuf = Lexing.from_string str in
  Input_parser.tasks Input_lexer.token lexbuf

let parse_file fname =
  let lines = Lib.load_file fname in
  let str = String.concat "\n" lines in
  parse_string str
