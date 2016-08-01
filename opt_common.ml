(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT licence           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* Common optimization functions and types                                    *)
(* -------------------------------------------------------------------------- *)

(* Optimization parameters (not all parameters are supported by individual backends) *)
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

let empty_result = {
  result = 0.0;
  lower_bound = 0.0;
  iters = 0;
  time = 0.0;
}

let get_tol name default =
  let tol = Config.get_float_option name in
  if tol < 0.0 then
    let () = Log.warning 0 "Bad tolerance value: %s = %e. Using default value: %e"
		         name tol default in
    default
  else
    tol

let default_opt_pars = {
  f_rel_tol = get_tol "opt-f-rel-tol" 0.01;
  f_abs_tol = get_tol "opt-f-abs-tol" 0.01;
  x_rel_tol = get_tol "opt-x-rel-tol" 0.0;
  x_abs_tol = get_tol "opt-x-abs-tol" 0.01;
  max_iters = Config.get_int_option "opt-max-iters";
  timeout = Config.get_int_option "opt-timeout";
}

let get_float strs name =
  let pat = name in
  let n = String.length pat in
  let r = Str.regexp pat in
  let rec find = function
    | [] -> raise Not_found
    | str :: t ->
      let i = try Str.search_forward r str 0 with Not_found -> -1 in
      if i == 0 then
	float_of_string (Str.string_after str n)
      else
	find t in
  find strs
