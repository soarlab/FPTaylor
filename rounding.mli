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

(* significand bits, (max_exp, min_exp) *)
val mk_value_type : int -> int * int -> value_type

(* significand bits (including the implicit bit) and exponent bits *)
val value_type_of_bits : int -> int -> value_type

val value_type_of_total_bits : int -> value_type

val real_type : value_type
val fp16_type : value_type
val fp32_type : value_type
val fp64_type : value_type
val fp128_type : value_type

val string_to_value_type : string -> value_type

val is_subtype : value_type -> value_type -> bool

val is_no_rnd : rnd_info -> bool

(* deprecated *)
val type_to_bits : value_type -> int

val type_precision : value_type -> int

val type_min_exp : value_type -> int

val create_rounding : value_type -> string -> float -> rnd_info
  
val create_explicit_rounding : value_type -> string -> float -> int -> int -> rnd_info

val string_to_rounding : string -> rnd_info

val value_type_to_string : value_type -> string

val rounding_to_string : rnd_info -> string

val get_eps : int -> float
