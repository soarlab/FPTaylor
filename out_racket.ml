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

module Out = ExprOut.Make(ExprOut.RacketPrinter)
       
let gen_racket_function fmt (name, total2, exp, e) =
  let nl = Format.pp_print_newline fmt in
  let p str = Format.pp_print_string fmt str; nl() in 
  let p' = Format.pp_print_string fmt in
  let var_names = vars_in_expr e in
  let vars = List.map (fun v -> v ^ "-var") var_names in
  p (Format.sprintf "(define eps (make-interval (bfexp2 (bf %d))))\n" exp);
  p (Format.sprintf "(define total2 (make-interval %e))\n" total2);
  p' (Format.sprintf "(define (%s " name);
  Lib.print_list p' (fun () -> p' " ") vars;
  p' ")\n\t";
  Out.print_fmt ~margin:80 fmt e;
  p' ")"

let create_racket_file fname name total2 exp expr =
  let tmp = Lib.get_tmp_dir () in
  let out_name = Filename.concat tmp (fname ^ ".rkt") in
  let _ = Lib.write_to_file out_name gen_racket_function (name, total2, exp, expr) in
  ()
