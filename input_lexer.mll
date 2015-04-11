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
  | ':' { COLON }
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
  | "float16" { FLOAT(16) }
  | "float32" { FLOAT(32) }
  | "float64" { FLOAT(64) }
  | "float128" { FLOAT(128) }
  | "rnd" { RND }
  | "no_rnd" { NO_RND }
  | "rnd16_ne" | "rnd16" { RND_PAR(16, "ne") }
  | "rnd16_0" { RND_PAR(16, "zero") }
  | "rnd16_down" { RND_PAR(16, "down") }
  | "rnd16_up" { RND_PAR(16, "up") }
  | "rnd32_ne" | "rnd32" { RND_PAR(32, "ne") }
  | "rnd32_0" { RND_PAR(32, "zero") }
  | "rnd32_down" { RND_PAR(32, "down") }
  | "rnd32_up" { RND_PAR(32, "up") }
  | "rnd64_ne" | "rnd64" { RND_PAR(64, "ne") }
  | "rnd64_0" { RND_PAR(64, "zero") }
  | "rnd64_down" { RND_PAR(64, "down") }
  | "rnd64_up" { RND_PAR(64, "up") }
  | "rnd128_ne" | "rnd128" { RND_PAR(128, "ne") }
  | "rnd128_0" { RND_PAR(128, "zero") }
  | "rnd128_down" { RND_PAR(128, "down") }
  | "rnd128_up" { RND_PAR(128, "up") }
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
  | "sub2" { SUB2 }
  | "floor_power2" { FLOOR_POWER2 }
  | "sym_interval" { SYM_INTERVAL }
  | id as str { ID str }
  | _ { token lexbuf }
  | eof { EOF }
