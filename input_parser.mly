%{
  open Input_parser_env
  open Rounding
  exception TODO
%}

%token EOF
%token COLON SEMICOLON COMMA
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
%token PLUS MINUS MULT DIVIDE POW
%token EQ LE GE LT GT IN PLUS_MINUS

%token <string> ID
%token <string> NUMBER SINGLE_NUMERAL DOUBLE_NUMERAL

%token CONSTANTS VARIABLES DEFINITIONS CONSTRAINTS EXPRESSIONS
%token INT REAL FLOAT
%token <Rounding.value_type> FLOAT_PAR
%token <Rounding.value_type * string> RND_PAR
%token RND NO_RND
%token E_CONST

%token ABS INV SQRT FMA
%token MIN MAX 
%token EXP LOG
%token COS SIN TAN COSH SINH TANH
%token ACOS ASIN ATAN ACOSH ASINH ATANH ATAN2
%token SUB2 FLOOR_POWER2

%left PLUS MINUS
%left MULT DIVIDE
%left NEG
%right POW

%start tasks
%type <Task.task list> tasks

%start expr
%type <Input_parser_env.raw_expr> expr

%type <Input_parser_env.raw_formula> raw_constr
%type <Rounding.rnd_info> rnd

%%

tasks:
  | task { $1 }
  | LBRACE task RBRACE more_tasks { $2 @ $4 }
;

more_tasks:
  | { [] }
  | LBRACE task RBRACE more_tasks { $2 @ $4 }
;

task:
  | section_list { 
    let tasks = env_to_tasks () in
    reset();
    tasks
  }
;

section_list:
  | {}
  | section section_list {}
;

section:
  | CONSTANTS constants_list {}
  | VARIABLES variables_list {}
  | DEFINITIONS definitions_list {}
  | CONSTRAINTS constraints_list {}
  | EXPRESSIONS expressions_list {}
;

separator:
  | COMMA {}
  | SEMICOLON {}
;

constants_list:
  | {}
  | constant separator constants_list {}
;

constant:
  ID EQ expr { add_constant $1 $3 }
;

variables_list:
  | {}
  | variable separator variables_list {}
;

variable:
  | var_type ID IN LBRACKET expr COMMA expr RBRACKET PLUS_MINUS expr 
    { add_variable_with_uncertainty $1 $2 $5 $7 $10 }
  | var_type ID IN LBRACKET expr COMMA expr RBRACKET 
    { add_variable $1 $2 $5 $7 }
;

var_type:
  | { string_to_value_type (Config.get_string_option "default-var-type") }
  | INT { real_type }
  | REAL { real_type }
  | FLOAT_PAR { $1 }
  | FLOAT value_type { $2 }
;

value_type:
  | LT NUMBER GT { value_type_of_total_bits (int_of_string $2) }
  | LT NUMBER COMMA NUMBER GT 
    { value_type_of_bits (int_of_string $2) (int_of_string $4) }
  | LT NUMBER COMMA LPAREN NUMBER COMMA NUMBER RPAREN GT 
    { mk_value_type (int_of_string $2) (int_of_string $5, int_of_string $7) }
;

definitions_list:
  | {}
  | definition separator definitions_list {}
;

definition:
  | ID EQ expr { add_definition $1 $3 }
  | ID rnd EQ expr { add_definition $1 (apply_raw_rounding $2 $4) }
;

constraints_list:
  | {}
  | constr separator constraints_list {}
;

constr:
  | ID COLON raw_constr { add_constraint $1 $3 }
  | ID rnd COLON raw_constr { add_constraint $1 (apply_raw_rounding_to_formula $2 $4) }
;

raw_constr:
  | expr EQ expr { Raw_eq ($1, $3) }
  | expr LE expr { Raw_le ($1, $3) }
  | expr LT expr { Raw_lt ($1, $3) }
  | expr GE expr { Raw_le ($3, $1) }
  | expr GT expr { Raw_lt ($3, $1) }
  | LPAREN raw_constr RPAREN { $2 }
;

expressions_list:
  | {}
  | expression separator expressions_list {}
;

expression:
  | ID EQ expr { add_expression_with_name $1 $3 }
  | ID rnd EQ expr { add_expression_with_name $1 (apply_raw_rounding $2 $4) }
  | expr { add_expression $1 }
;

pos_neg_number:
  | NUMBER { $1 }
  | MINUS NUMBER { ("-" ^ $2) }
;

rnd:
  | RND LBRACKET value_type COMMA ID COMMA NUMBER COMMA pos_neg_number COMMA pos_neg_number RBRACKET
    { create_explicit_rounding $3 ($5) (float_of_string $7) (int_of_string $9) (int_of_string $11) }
  | RND LBRACKET value_type COMMA ID COMMA NUMBER RBRACKET
    { create_rounding $3 ($5) (float_of_string $7) }
  | RND LBRACKET value_type COMMA ID RBRACKET
    { create_rounding $3 ($5) 1.0 }
  | NO_RND { create_rounding (mk_value_type 0 (0, 0)) "ne" 1.0 }
  | RND LBRACKET NUMBER COMMA ID COMMA NUMBER COMMA pos_neg_number COMMA pos_neg_number RBRACKET
    { (* deprecated *) 
        create_explicit_rounding (value_type_of_total_bits (int_of_string $3)) ($5) 
        (float_of_string $7) (int_of_string $9) (int_of_string $11) }
  | RND LBRACKET NUMBER COMMA ID COMMA NUMBER RBRACKET
    { (* deprecated *)
      create_rounding (value_type_of_total_bits (int_of_string $3)) ($5) (float_of_string $7) }
  | RND LBRACKET NUMBER COMMA ID RBRACKET
    { (* deprecated *)
      create_rounding (value_type_of_total_bits (int_of_string $3)) ($5) 1.0 }
  | RND_PAR
    { create_rounding (fst $1) (snd $1) 1.0 }
  | RND value_type
    { create_rounding $2 "ne" 1.0 }
  | RND
    { string_to_rounding (Config.get_string_option "default-rnd") }
;

expr:
  | ID { Identifier $1 }
  | NUMBER { Numeral (More_num.num_of_float_string $1) }
  | SINGLE_NUMERAL
    { Raw_rounding (string_to_rounding "rnd32", Numeral (More_num.num_of_float_string $1)) }
  | DOUBLE_NUMERAL 
    { Raw_rounding (string_to_rounding "rnd64", Numeral (More_num.num_of_float_string $1)) }
  | rnd LPAREN expr RPAREN { Raw_rounding ($1, $3) }
  | expr PLUS expr { Raw_bin_op ("+", $1, $3) }
  | expr MINUS expr { Raw_bin_op ("-", $1, $3) }
  | expr MULT expr { Raw_bin_op ("*", $1, $3) }
  | expr DIVIDE expr { Raw_bin_op ("/", $1, $3) }
  | MINUS expr %prec NEG { Raw_u_op ("-", $2) }
  | PLUS expr %prec NEG { $2 }
  | E_CONST POW expr { Raw_u_op ("exp", $3) }
  | expr POW NUMBER { Raw_bin_op ("^", $1, Numeral (Num.num_of_string $3)) }
  | expr POW LPAREN MINUS NUMBER RPAREN 
    { Raw_bin_op ("^", $1, Numeral (Num.minus_num (Num.num_of_string $5))) }
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
  | MIN LPAREN expr COMMA expr RPAREN { Raw_bin_op ("min", $3, $5) }
  | MAX LPAREN expr COMMA expr RPAREN { Raw_bin_op ("max", $3, $5) }
  | COSH LPAREN expr RPAREN { Raw_u_op ("cosh", $3) }
  | SINH LPAREN expr RPAREN { Raw_u_op ("sinh", $3) }
  | TANH LPAREN expr RPAREN { Raw_u_op ("tanh", $3) }
  | ACOS LPAREN expr RPAREN { Raw_u_op ("acos", $3) }
  | ASIN LPAREN expr RPAREN { Raw_u_op ("asin", $3) }
  | ATAN LPAREN expr RPAREN { Raw_u_op ("atan", $3) }
  | ACOSH LPAREN expr RPAREN { Raw_u_op ("acosh", $3) }
  | ASINH LPAREN expr RPAREN { Raw_u_op ("asinh", $3) }
  | ATANH LPAREN expr RPAREN { Raw_u_op ("atanh", $3) }
  | ATAN2 LPAREN expr COMMA expr RPAREN { raise TODO }
  | SUB2 LPAREN expr COMMA expr RPAREN { Raw_bin_op ("sub2", $3, $5) }
  | FLOOR_POWER2 LPAREN expr RPAREN { Raw_u_op ("floor_power2", $3) }
;

%%
