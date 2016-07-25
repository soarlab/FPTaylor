val base_dir : string
val input_files : string list

val get_string_option : string -> string
val get_bool_option : string -> bool
val get_int_option : string -> int
val get_float_option : string -> float

val debug : bool
val fail_on_exception : bool
val opt_tol : float
val proof_flag : bool
val verbosity : int

val print_options : Format.formatter -> unit

