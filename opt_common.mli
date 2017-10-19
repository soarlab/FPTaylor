(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT license           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* Common optimization functions and types                                    *)
(* -------------------------------------------------------------------------- *)

type opt_pars = {
  f_rel_tol : float;
  f_abs_tol : float;
  x_rel_tol : float;
  x_abs_tol : float;
  max_iters : int;
  timeout : int;
}

type opt_result = {
  result : float;
  lower_bound : float;
  iters : int;
  time : float;
}

val empty_result : opt_result

val default_opt_pars : opt_pars
  
val get_float : ?default:float -> string list -> string -> float
