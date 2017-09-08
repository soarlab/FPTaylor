{
  open Fpbench_parser2
  open Lexing

  exception SyntaxError of string

  let incr_lineno lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum;
    }
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let special = ['_' '~' '!' '@' '$' '%' '^' '&' '*' '-' '+' '=' '<' '>' '.' '?' '/' ':']

let symbol = (alpha | special) (alpha | special | digit)*

let number = ['-' '+']? digit+ ('.' digit+ ('e' ['-' '+']? digit+)?)?

rule token = parse
  | white { token lexbuf }
  | newline { incr_lineno lexbuf; token lexbuf }
  | ';' [^ '\n']* { token lexbuf }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | '"' { read_string (Buffer.create 50) lexbuf } 
  | number as str { NUMBER str }
  | ':' { COLON }
  | symbol as str { SYMBOL str }
(*  | _ { token lexbuf } *)
  | _ { raise (SyntaxError ("Unexpected character: " ^ Lexing.lexeme lexbuf)) }
  | eof { EOF }
and read_string buf = parse
  | '"' { STRING (Buffer.contents buf) }
  | '\\' '\\' { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '"' { Buffer.add_char buf '"'; read_string buf lexbuf }
  | [^ '"' '\\']+ as chars { Buffer.add_string buf chars; read_string buf lexbuf }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }