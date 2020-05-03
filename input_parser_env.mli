type raw_expr =
  | Identifier of string
  | Numeral of Num.num
  | Raw_rounding of Rounding.rnd_info * raw_expr
  | Raw_u_op of string * raw_expr
  | Raw_bin_op of string * raw_expr * raw_expr
  | Raw_gen_op of string * raw_expr list

type raw_formula =
  | Raw_le of raw_expr * raw_expr
  | Raw_lt of raw_expr * raw_expr
  | Raw_eq of raw_expr * raw_expr

val reset : unit -> unit

val env_to_tasks : unit -> Task.task list

val add_constant : string -> raw_expr -> unit

val add_variable : Rounding.value_type -> string -> raw_expr * raw_expr -> unit

val add_variable_with_uncertainty : Rounding.value_type -> string -> raw_expr * raw_expr * raw_expr -> unit

val add_definition : string -> raw_expr -> Expr.expr

val add_expression_with_name : string -> raw_expr -> unit

val add_expression : raw_expr -> unit

val add_constraint : string -> raw_formula -> unit

val apply_raw_rounding : Rounding.rnd_info -> raw_expr -> raw_expr

val apply_raw_rounding_to_formula : Rounding.rnd_info -> raw_formula -> raw_formula

val transform_raw_expr : raw_expr -> Expr.expr