type token =
  | EOF
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | NUMBER of string
  | SYMBOL of string
  | STRING of string

exception SyntaxError of string

type s_expr

val print_s_expr : s_expr -> unit

val parse_s_expr : ?first_tok:token -> (Lexing.lexbuf -> token) -> Lexing.lexbuf -> s_expr

type data = {
  name : string;
  precision : Rounding.rnd_info;
  vars : (string * (Environment.raw_expr option * Environment.raw_expr option)) list;
  constraints : Environment.raw_formula list;
  expr : Environment.raw_expr;
}
  
val translate_fpcore : s_expr -> data
