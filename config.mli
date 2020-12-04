(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT license           *)
(* ========================================================================== *)

val base_dir : unit -> string
val input_files : unit -> string list

val set_option : ?init:bool -> ?short:string -> string -> string -> unit
                         
val get_string_option : string -> string
val get_bool_option : string -> bool
val get_int_option : string -> int
val get_float_option : string -> float

val is_option_defined : string -> bool

val debug : unit -> bool
val fail_on_exception : unit -> bool
val proof_flag : unit -> bool

val export_options : Format.formatter -> unit

val print_options : level:Log.level -> unit
val print_short_names : level:Log.level -> unit

val init : ?main_cfg_fname:string -> ?usage:string -> string list -> unit