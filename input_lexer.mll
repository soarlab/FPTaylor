{
  open Input_parser
  open Lexing

  let incr_lineno lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum;
    }
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z' '_']
let id = alpha (alpha | digit)*
let number = (digit+ ('.' digit*)?)('e'['-' '+']?digit+)?

rule token = parse
  | "//" [^ '\n']*
  | [' ' '\t'] { token lexbuf }
  | '\n' { incr_lineno lexbuf; token lexbuf }
  | "Constants" 
  | "constants" { CONSTANTS }
  | "Variables"
  | "variables" { VARIABLES }
  | "Definitions"
  | "definitions" { DEFINITIONS }
  | "Constraints"
  | "constraints" { CONSTRAINTS }
  | "Expressions"
  | "expressions" { EXPRESSIONS }
  | number as str { NUMBER str }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | ';' { SEMICOLON }
  | ',' { COMMA }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { MULT }
  | '/' { DIVIDE }
  | '^' { POW }
  | '=' { EQ }
  | "<=" { LE }
  | ">=" { GE }
  | "<" { LT }
  | ">" { GT }
  | "+/-" { PLUS_MINUS }
  | "in" 
  | "IN" { IN }
  | "int" { INT }
  | "real" { REAL }
  | "inv" { INV }
  | "abs" { ABS }
  | "fma" { FMA }
  | "sqrt" { SQRT }
  | "min" { MIN }
  | "max" { MAX }
  | "%e" { E_CONST }
  | "exp" { EXP }
  | "log" { LOG }
  | "cos" { COS }
  | "sin" { SIN }
  | "tan" { TAN }
  | "cosh" { COSH }
  | "sinh" { SINH }
  | "tanh" { TANH }
  | "acos" { ACOS }
  | "asin" { ASIN }
  | "atan" { ATAN }
  | "acosh" { ACOSH }
  | "asinh" { ASINH }
  | "atanh" { ATANH }
  | "atan2" { ATAN2 }
  | id as str { ID str }
  | _ { token lexbuf }
  | eof { EOF }
