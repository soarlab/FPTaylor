val base_dir : string
val input_files : string list

val get_string_option : string -> string
val get_bool_option : string -> bool
val get_int_option : string -> int
val get_float_option : string -> float

val debug : bool
val fail_on_exception : bool
val uncertainty : bool
val subnormal : bool
val const_approx_real_vars : bool
val simplification : bool
val fp_power2_model : bool
val rel_error : bool
val abs_error : bool
val opt : string
val opt_approx : bool
val opt_exact : bool
val opt_tol : float
val proof_flag : bool
val verbosity : int

val print_options : Format.formatter -> unit

