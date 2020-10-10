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

let print_position () lexbuf =
  let open Lexing in
  let pos = lexbuf.lex_curr_p in
  let lex = Lexing.lexeme lexbuf in
  let cend = pos.pos_cnum - pos.pos_bol + 1 in
  let cstart = cend - String.length lex in
  Printf.sprintf "%sline %d, characters %d-%d (%s)"
    (if pos.pos_fname = "" then "" else pos.pos_fname ^ ", ")
    pos.pos_lnum cstart cend lex

let parse_with_errors lexbuf parse =
  try parse Input_lexer.token lexbuf with
  | Input_lexer.SyntaxError msg ->
    Log.error "%a: %s" print_position lexbuf msg;
    raise Parsing.Parse_error
  | Parsing.Parse_error ->
    Log.error "%a: syntax error" print_position lexbuf;
    raise Parsing.Parse_error

let create_env_from_task task =
  Input_parser_env.create_env_from_task task

let parse_raw_expr str =
  parse_with_errors (Lexing.from_string str) Input_parser.expr

let parse_expr str =
  let raw = parse_raw_expr str in
  Input_parser_env.transform_raw_expr raw

let parse_string ?(fname = "") str =
  Input_parser_env.reset();
  let lexbuf = Lexing.from_string str in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = fname };
  parse_with_errors lexbuf Input_parser.tasks

let parse_file fname =
  let lines = Lib.load_file fname in
  let str = String.concat "\n" lines in
  parse_string ~fname str
