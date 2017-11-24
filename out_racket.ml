(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT license           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* Racket output for FPTaylor expressions                                     *)
(* -------------------------------------------------------------------------- *)

open Expr

module Out = ExprOut.Make(ExprOut.RacketIntervalPrinter)

let gen_racket_function fmt (name, cs, total2, exp, e, opt_bound) =
  let n2s = Num.string_of_num in
  let f2s f = n2s (More_num.num_of_float f) in
  let p' = Format.pp_print_string fmt in
  let var_names = vars_in_expr e in
  let var_bounds = List.map cs.var_rat_bounds var_names in
  let bound_strings =
    List.map (fun (low, high) -> 
      Format.sprintf "(cons %s %s)" (n2s low) (n2s high))
      var_bounds in
  let vars = List.map (fun v -> v ^ "-var") var_names in
  Format.fprintf fmt "(define name \"%s\")@." name;
  Format.fprintf fmt "(define opt-max %s)@."
    (match opt_bound with None -> "#f" | Some f -> f2s f);
  Format.fprintf fmt "(define bounds (list %s))@." (String.concat " " bound_strings);
  Format.fprintf fmt "(define eps (make-interval (bfexp2 (bf %d))))@." exp;
  Format.fprintf fmt "(define total2 (make-interval %s))@." (f2s total2);
  p' "(define (fptaylor-model ";
  Lib.print_list p' (fun () -> p' " ") vars;
  p' ")\n\t";
  Out.print_fmt ~margin:80 fmt e;
  p' ")"

let create_racket_file fmt ~name cs total2 exp expr opt_bound =
  gen_racket_function fmt (name, cs, total2, exp, expr, opt_bound)
