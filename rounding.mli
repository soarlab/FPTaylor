(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT license           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* Parameters for rounding operations                                         *)
(* -------------------------------------------------------------------------- *)

type value_type

type rnd_type = Rnd_ne | Rnd_up | Rnd_down | Rnd_0

type rnd_info = {
  (* Approximation of the maximum value *)
  max_val : float;
  eps_exp : int;
  delta_exp : int;
  coefficient : float;
  fp_type : value_type;
  rnd_type : rnd_type;
  special_flag : bool;
}

val mk_value_type : int -> value_type

val real_type : value_type

val string_to_value_type : string -> value_type

val value_type_to_string : value_type -> string

val is_subtype : value_type -> value_type -> bool

val is_no_rnd : rnd_info -> bool

val type_size : value_type -> int

val type_precision : value_type -> int

val type_min_exp : value_type -> int

val create_rounding : int -> string -> float -> rnd_info
  
val create_explicit_rounding : int -> string -> float -> int -> int -> rnd_info

val string_to_rounding : string -> rnd_info

val rounding_to_string : rnd_info -> string

val get_eps : int -> float
