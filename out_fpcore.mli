(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT license           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* FPCore output for FPTaylor tasks                                           *)
(* -------------------------------------------------------------------------- *)

val generate_fpcore : Format.formatter -> ?var_type:Rounding.value_type -> Task.task -> unit
