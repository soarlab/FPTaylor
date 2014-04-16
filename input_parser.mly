%{
  open Environment
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

%token ABS INV SQRT FMA
%token MIN MAX 
%token EXP LOG
%token COS SIN TAN COSH SINH TANH
%token ACOS ASIN ATAN ACOSH ASINH ATANH ATAN2

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
  { add_variable_with_uncertainty $2 $5 $7 $10 }
  | var_type ID IN LBRACKET expr COMMA expr RBRACKET 
      { add_variable $2 $5 $7 }
;

var_type: /* empty */ {}
  | INT {}
  | REAL {}
;

definitions:
  definition next_definitions {}
;

next_definitions: /* empty */ {}
  | COMMA definitions {}
;

definition:
  ID EQ expr { add_definition $1 $3 }
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

expr:
  ID { Identifier $1 }
  | INTEGER { Numeral $1 }
  | NUMBER { Numeral $1 }
  | expr PLUS expr { Raw_bin_op ("+", false, $1, $3) }
  | expr MINUS expr { Raw_bin_op ("-", false, $1, $3) }
  | expr MULT expr { Raw_bin_op ("*", false, $1, $3) }
  | expr DIVIDE expr { Raw_bin_op ("/", false, $1, $3) }
  | MINUS expr %prec NEG { Raw_u_op ("-", false, $2) }
  | PLUS expr %prec NEG { $2 }
  | expr POW NUMBER { Raw_bin_op ("^", false, $1, Numeral $3) }
  | LPAREN expr RPAREN { $2 }
  | ABS LPAREN expr RPAREN { Raw_u_op ("abs", false, $3) }
  | INV LPAREN expr RPAREN { Raw_u_op ("inv", false, $3) }
  | FMA LPAREN expr COMMA expr COMMA expr RPAREN 
      { Raw_gen_op ("fma", false, [$3; $5; $7]) }
  | SQRT LPAREN expr RPAREN { Raw_u_op ("sqrt", false, $3) }
  | EXP LPAREN expr RPAREN { Raw_u_op ("exp", false, $3) }
  | LOG LPAREN expr RPAREN { Raw_u_op ("log", false, $3) }
  | COS LPAREN expr RPAREN { Raw_u_op ("cos", false, $3) }
  | SIN LPAREN expr RPAREN { Raw_u_op ("sin", false, $3) }
  | TAN LPAREN expr RPAREN { Raw_u_op ("tan", false, $3) }
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
;

%%
