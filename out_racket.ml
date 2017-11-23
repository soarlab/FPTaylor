(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT license           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* S-expression output for FPTaylor expressions                               *)
(* -------------------------------------------------------------------------- *)

open Expr

module Out = ExprOut.Make(ExprOut.RacketIntervalPrinter)

let gen_racket_function fmt (cs, total2, exp, e) =
  let n2s = Num.string_of_num in
  let p' = Format.pp_print_string fmt in
  let var_names = vars_in_expr e in
  let var_bounds = List.map cs.var_rat_bounds var_names in
  let bound_strings =
    List.map (fun (low, high) -> 
      Format.sprintf "(cons %s %s)" (n2s low) (n2s high))
      var_bounds in
  let vars = List.map (fun v -> v ^ "-var") var_names in
  Format.fprintf fmt "(define bounds (list %s))@." (String.concat " " bound_strings);
  Format.fprintf fmt "(define eps (make-interval (bfexp2 (bf %d))))@." exp;
  Format.fprintf fmt "(define total2 (make-interval %s))@." (n2s (More_num.num_of_float total2));
  p' "(define (fptaylor-model ";
  Lib.print_list p' (fun () -> p' " ") vars;
  p' ")\n\t";
  Out.print_fmt ~margin:80 fmt e;
  p' ")"

let create_racket_file fname cs total2 exp expr =
  let tmp = Lib.get_tmp_dir () in
  let out_name = Filename.concat tmp (fname ^ ".rkt") in
  let _ = Lib.write_to_file out_name gen_racket_function (cs, total2, exp, expr) in
  ()
