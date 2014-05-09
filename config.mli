val base_dir : string
(* arguments: option name -> default value *)
val get_option : string -> string -> string
val get_bool_option : string -> bool -> bool
val get_int_option : string -> int -> int
val get_float_option : string -> float -> float
val input_files : string list
val uncertainty : bool
val subnormal : bool
val real_vars : bool
val const_approx_real_vars : bool
val simplification : bool
val fp : int
val rounding : string
val rel_error : bool
val abs_error : bool
val opt : string
val opt_approx : bool
val opt_tol : float
val print_options : Format.formatter -> unit

