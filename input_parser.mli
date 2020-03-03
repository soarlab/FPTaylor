type token =
  | EOF
  | COLON
  | SEMICOLON
  | COMMA
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | LBRACE
  | RBRACE
  | PLUS
  | MINUS
  | MULT
  | DIVIDE
  | POW
  | EQ
  | LE
  | GE
  | LT
  | GT
  | IN
  | PLUS_MINUS
  | ID of (string)
  | NUMBER of (string)
  | SINGLE_NUMERAL of (string)
  | DOUBLE_NUMERAL of (string)
  | CONSTANTS
  | VARIABLES
  | DEFINITIONS
  | CONSTRAINTS
  | EXPRESSIONS
  | INT
  | REAL
  | FLOAT of (int)
  | RND_PAR of (int * string)
  | RND
  | NO_RND
  | E_CONST
  | ABS
  | INV
  | SQRT
  | FMA
  | MIN
  | MAX
  | EXP
  | LOG
  | COS
  | SIN
  | TAN
  | COSH
  | SINH
  | TANH
  | RELU
  | ACOS
  | ASIN
  | ATAN
  | ACOSH
  | ASINH
  | ATANH
  | ATAN2
  | SUB2
  | FLOOR_POWER2

val tasks :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Task.task list
val expr :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Input_parser_env.raw_expr
