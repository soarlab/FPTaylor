%{
  open Environment
  open Rounding
  exception TODO
%}

%token EOF
%token SEMICOLON COMMA
%token LPAREN RPAREN LBRACKET RBRACKET
%token PLUS MINUS MULT DIVIDE POW
%token EQ LE GE LT GT IN PLUS_MINUS

%token <string> ID
%token <string> INTEGER
%token <string> NUMBER

%token CONSTANTS VARIABLES DEFINITIONS CONSTRAINTS EXPRESSIONS
%token INT REAL 
%token <int> FLOAT
%token <int * string> RND_PAR
%token RND
%token E_CONST

%token ABS INV SQRT FMA
%token MIN MAX 
%token EXP LOG
%token COS SIN TAN COSH SINH TANH
%token ACOS ASIN ATAN ACOSH ASINH ATANH ATAN2
%token FLOOR_POWER2 SYM_INTERVAL

%left PLUS MINUS
%left MULT DIVIDE
%left NEG
%right POW

%start first
%type <unit> first

%start expr
%type <Environment.raw_expr> expr

%%

first: 
  pragma SEMICOLON next_pragma {}
;

next_pragma: /* empty */ {}
  | first {}
;

pragma:
  CONSTANTS constants {}
  | VARIABLES variables {}
  | DEFINITIONS definitions {}
  | CONSTRAINTS constraints {}
  | EXPRESSIONS expressions {}
;

constants:
  constant next_constants {}
;

next_constants: /* empty */ {}
  | COMMA constants {}
;

constant:
  ID EQ expr { add_constant $1 $3 }
;

variables:
  variable next_variables {}
;

next_variables: /* empty */ {}
  | COMMA variables {}
;

variable:
  var_type ID IN LBRACKET expr COMMA expr RBRACKET PLUS_MINUS expr 
  { add_variable_with_uncertainty $1 $2 $5 $7 $10 }
  | var_type ID IN LBRACKET expr COMMA expr RBRACKET 
      { add_variable $1 $2 $5 $7 }
;

var_type: /* empty */ { real_type }
  | INT { real_type }
  | REAL { real_type }
  | FLOAT { mk_value_type $1 }
;

definitions:
  definition next_definitions {}
;

next_definitions: /* empty */ {}
  | COMMA definitions {}
;

definition:
  ID EQ expr { add_definition $1 $3 }
  | ID rnd EQ expr { add_definition $1 (apply_raw_rounding $2 $4) }
;

constraints:
  constr next_constraints {}
;

next_constraints: /* empty */ {}
  | COMMA constraints {}
;

constr:
  expr EQ expr { add_constraint (Raw_eq ($1, $3)) }
  | expr LE expr { add_constraint (Raw_le ($1, $3)) }
  | expr LT expr { add_constraint (Raw_lt ($1, $3)) }
  | expr GE expr { add_constraint (Raw_le ($3, $1)) }
  | expr GT expr { add_constraint (Raw_lt ($3, $1)) }
  | expr IN LBRACKET expr COMMA expr RBRACKET { 
    add_constraint (Raw_le ($1, $6));
    add_constraint (Raw_le ($4, $1))
  }
;

expressions:
  expr next_expressions { add_expression $1 }
;

next_expressions: /* empty */ {}
  | COMMA expressions {}
;

pos_neg_number:
  NUMBER { $1 }
  | MINUS NUMBER { ("-" ^ $2) }
;

rnd:
  RND LPAREN NUMBER COMMA ID COMMA NUMBER COMMA pos_neg_number COMMA pos_neg_number RPAREN
  { create_explicit_rounding (int_of_string $3) ($5) 
      (float_of_string $7) (int_of_string $9) (int_of_string $11) }
  | RND LPAREN NUMBER COMMA ID COMMA NUMBER RPAREN
  { create_rounding (int_of_string $3) ($5) (float_of_string $7) }
  | RND LPAREN NUMBER COMMA ID RPAREN
      { create_rounding (int_of_string $3) ($5) 1.0 }
  | RND_PAR
      { create_rounding (fst $1) (snd $1) 1.0 }
;

expr:
  ID { Identifier $1 }
  | INTEGER { Numeral $1 }
  | NUMBER { Numeral $1 }
  | rnd LPAREN expr RPAREN { Raw_rounding ($1, $3) }
  | expr PLUS expr { Raw_bin_op ("+", $1, $3) }
  | expr MINUS expr { Raw_bin_op ("-", $1, $3) }
  | expr MULT expr { Raw_bin_op ("*", $1, $3) }
  | expr DIVIDE expr { Raw_bin_op ("/", $1, $3) }
  | MINUS expr %prec NEG { Raw_u_op ("-", $2) }
  | PLUS expr %prec NEG { $2 }
  | E_CONST POW expr { Raw_u_op ("exp", $3) }
  | expr POW NUMBER { Raw_bin_op ("^", $1, Numeral $3) }
  | expr POW LPAREN MINUS NUMBER RPAREN { Raw_bin_op ("^", $1, Numeral ("-" ^ $5)) }
  | LPAREN expr RPAREN { $2 }
  | ABS LPAREN expr RPAREN { Raw_u_op ("abs", $3) }
  | INV LPAREN expr RPAREN { Raw_u_op ("inv", $3) }
  | FMA LPAREN expr COMMA expr COMMA expr RPAREN 
      { Raw_gen_op ("fma", [$3; $5; $7]) }
  | SQRT LPAREN expr RPAREN { Raw_u_op ("sqrt", $3) }
  | EXP LPAREN expr RPAREN { Raw_u_op ("exp", $3) }
  | LOG LPAREN expr RPAREN { Raw_u_op ("log", $3) }
  | COS LPAREN expr RPAREN { Raw_u_op ("cos", $3) }
  | SIN LPAREN expr RPAREN { Raw_u_op ("sin", $3) }
  | TAN LPAREN expr RPAREN { Raw_u_op ("tan", $3) }
  | MIN LPAREN expr COMMA expr RPAREN { raise TODO }
  | MAX LPAREN expr COMMA expr RPAREN { raise TODO }
  | COSH LPAREN expr RPAREN { raise TODO }
  | SINH LPAREN expr RPAREN { raise TODO }
  | TANH LPAREN expr RPAREN { raise TODO }
  | ACOS LPAREN expr RPAREN { raise TODO }
  | ASIN LPAREN expr RPAREN { raise TODO }
  | ATAN LPAREN expr RPAREN { raise TODO }
  | ACOSH LPAREN expr RPAREN { raise TODO }
  | ASINH LPAREN expr RPAREN { raise TODO }
  | ATANH LPAREN expr RPAREN { raise TODO }
  | ATAN2 LPAREN expr COMMA expr RPAREN { raise TODO }
  | FLOOR_POWER2 LPAREN expr RPAREN { Raw_u_op ("floor_power2", $3) }
  | SYM_INTERVAL LPAREN expr RPAREN { Raw_u_op ("sym_interval", $3) }
;

%%
