(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT license           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* Proof certificate data structures and constructors                         *)
(* -------------------------------------------------------------------------- *)

open Num

include (module type of Proof_base)

val mk_rnd_info : int -> float -> rnd_proof_info

val mk_proof_args : int list -> int list -> float list -> proof_args

val mk_proof_opt : proof_opt_type -> int list -> float list -> float -> proof_opt

val add_proof_step : int -> proof_op -> proof_args -> unit

val add_proof_opt : proof_opt -> unit

val new_proof : Task.task -> unit

val add_var_step : int -> string -> unit
  
val add_const_step : int -> num -> unit

val add_rnd_bin_var_step : int -> string -> rnd_proof_info -> int -> float -> int -> unit

val add_rnd_bin_const_step : int -> num -> rnd_proof_info -> int -> float -> int -> unit

val add_rnd_step : int -> rnd_proof_info -> int -> float -> float -> int -> int -> unit

val add_simpl_eq_step : int -> int -> int -> int -> unit

val add_simpl_add_step : int -> int -> int -> int -> int -> float -> int -> unit

val add_neg_step : int -> int -> unit

val add_add_step : int -> int -> int -> unit
  
val add_sub_step : int -> int -> int -> unit

val add_mul_step : int -> int -> int -> float -> float -> int -> unit

