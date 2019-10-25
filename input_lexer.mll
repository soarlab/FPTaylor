{
  open Input_parser
  open Lexing

  let incr_lineno lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum;
    }

  let resolve_id =
    let table = Hashtbl.create 100 in
    let () = List.iter (fun (k, v) -> Hashtbl.add table k v) [
      "Constants", CONSTANTS;
      "constants", CONSTANTS;
      "Variables", VARIABLES;
      "variables", VARIABLES;
      "Definitions", DEFINITIONS;
      "definitions", DEFINITIONS;
      "Constraints", CONSTRAINTS;
      "constraints", CONSTRAINTS;
      "Expressions", EXPRESSIONS;
      "expressions", EXPRESSIONS;
      "IN", IN;
      "in", IN;
      "int", INT;
      "real", REAL;
      "float", FLOAT;
      "float16", FLOAT_PAR(Rounding.fp16_type);
      "float32", FLOAT_PAR(Rounding.fp32_type);
      "float64", FLOAT_PAR(Rounding.fp64_type);
      "float128", FLOAT_PAR(Rounding.fp128_type);
      "rnd", RND;
      "no_rnd", NO_RND;
      "rnd16_ne", RND_PAR(Rounding.fp16_type, "ne"); 
      "rnd16", RND_PAR(Rounding.fp16_type, "ne"); 
      "rnd16_0", RND_PAR(Rounding.fp16_type, "zero");
      "rnd16_down", RND_PAR(Rounding.fp16_type, "down");
      "rnd16_up", RND_PAR(Rounding.fp16_type, "up");
      "rnd32_ne", RND_PAR(Rounding.fp32_type, "ne");
      "rnd32", RND_PAR(Rounding.fp32_type, "ne");
      "rnd32_0", RND_PAR(Rounding.fp32_type, "zero");
      "rnd32_down", RND_PAR(Rounding.fp32_type, "down");
      "rnd32_up", RND_PAR(Rounding.fp32_type, "up");
      "rnd64_ne", RND_PAR(Rounding.fp64_type, "ne");
      "rnd64", RND_PAR(Rounding.fp64_type, "ne");
      "rnd64_0", RND_PAR(Rounding.fp64_type, "zero");
      "rnd64_down", RND_PAR(Rounding.fp64_type, "down");
      "rnd64_up", RND_PAR(Rounding.fp64_type, "up");
      "rnd128_ne", RND_PAR(Rounding.fp128_type, "ne");
      "rnd128", RND_PAR(Rounding.fp128_type, "ne");
      "rnd128_0", RND_PAR(Rounding.fp128_type, "zero");
      "rnd128_down", RND_PAR(Rounding.fp128_type, "down");
      "rnd128_up", RND_PAR(Rounding.fp128_type, "up");
      "inv", INV;
      "abs", ABS;
      "fma", FMA;
      "sqrt", SQRT;
      "min", MIN;
      "max", MAX;
      "exp", EXP;
      "log", LOG;
      "cos", COS;
      "sin", SIN;
      "tan", TAN;
      "cosh", COSH;
      "sinh", SINH;
      "tanh", TANH;
      "acos", ACOS;
      "asin", ASIN;
      "atan", ATAN;
      "atan2", ATAN2;
      "arccos", ACOS;
      "arcsin", ASIN;
      "arctan", ATAN;
      "acosh", ACOSH;
      "asinh", ASINH;
      "atanh", ATANH;
      "arsinh", ASINH;
      "arcosh", ACOSH;
      "artanh", ATANH;
      "arcsinh", ASINH;
      "arccosh", ACOSH;
      "arctanh", ATANH;
      "argsinh", ASINH;
      "argcosh", ACOSH;
      "argtanh", ATANH;
      "sub2", SUB2;
      "floor_power2", FLOOR_POWER2;
    ] in
  fun str ->
    try Hashtbl.find table str
    with Not_found -> ID str

}

let digit = ['0'-'9']
let hex_digit = ['0'-'9' 'a'-'f' 'A'-'F']
let alpha = ['a'-'z' 'A'-'Z' '_' '$']
let id = alpha (alpha | digit)*
let numeral = (("0x" (hex_digit+ ('.' hex_digit*)?)) |
               (digit+ ('.' digit*)?)) 
              (['e' 'p'] ['-' '+']? digit+)?
let double_numeral = numeral 'd'
let single_numeral = numeral 'f'

rule token = parse
  | "//" [^ '\n']*
  | [' ' '\t'] { token lexbuf }
  | '\n' { incr_lineno lexbuf; token lexbuf }
  | single_numeral as str { SINGLE_NUMERAL (String.sub str 0 (String.length str - 1)) }
  | double_numeral as str { DOUBLE_NUMERAL (String.sub str 0 (String.length str - 1)) }
  | numeral as str { NUMBER str }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | '{' { LBRACE }
  | '}' { RBRACE }
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
  | "%e" { E_CONST }
  | id as str { resolve_id str }
  | _ { token lexbuf }
  | eof { EOF }
