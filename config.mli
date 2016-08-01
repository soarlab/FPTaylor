val base_dir : string
val input_files : string list

val add_option : ?init:bool -> ?short:string -> string -> string -> unit
                         
val get_string_option : string -> string
val get_bool_option : string -> bool
val get_int_option : string -> int
val get_float_option : string -> float

val debug : bool
val fail_on_exception : bool
val proof_flag : bool

val print_options : level:int -> unit
val print_short_names : level:int -> unit
