(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT license           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* Optimization with the GELPIA tool                                          *)
(* -------------------------------------------------------------------------- *)

open Interval
open Expr

module Out = ExprOut.Make(ExprOut.GelpiaPrinter)
       
let gen_test_code fmt =
  let nl = Format.pp_print_newline fmt in
  let p str = Format.pp_print_string fmt str; nl () in
  let p' str = Format.pp_print_string fmt str in
  let variables names bounds =
    let vars = List.map2 (fun name b ->
      Format.sprintf "  %s in [%.20e, %.20e]" name b.low b.high) 
      names bounds in
    p "Variables";
    p (String.concat ",\n" vars);
    p ";" 
  in
  let goal expr = 
    p "Maximize";
    p' "  goal = ";
    Out.print_fmt fmt expr;
    p ";"
  in
  fun (cs, expr) ->
    let var_names = vars_in_expr expr in
    let var_bounds = List.map cs.var_interval var_names in
    variables var_names var_bounds;
    p "";
    goal expr

let create_test_file fname cs expr =
  let tmp = Lib.get_tmp_dir () in
  let out_name = Filename.concat tmp fname in
  let _ = Lib.write_to_file out_name gen_test_code (cs, expr) in
  ()

