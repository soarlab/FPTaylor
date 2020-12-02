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

open Rounding
open Expr
open Task
open Format

module Out = ExprOut.Make(ExprOut.FPCorePrinter)

let sep fmt str = fun () -> pp_print_string fmt str

(* TODO: it is assumed that formulas do not contain rounding operations *)
let print_formula fmt formula =
  let print_expr = Out.print_fmt ~margin:max_int in
  match formula with
    | Le (a, b) -> fprintf fmt "(<= %a %a)" print_expr a print_expr b
    | Lt (a, b) -> fprintf fmt "(< %a %a)" print_expr a print_expr b
    | Eq (a, b) -> fprintf fmt "(== %a %a)" print_expr a print_expr b

let precision_of_value_type ty =
  match value_type_to_string ty with
  | "real" -> "real"
  | v -> Str.(replace_first (regexp "float") "binary" v)

let precision_of_rounding rnd = precision_of_value_type rnd.fp_type

let rm_of_rounding rnd =
  match rnd.rnd_type with
  | Rnd_ne -> "nearestEven"
  | Rnd_up -> "toPositive"
  | Rnd_down -> "toNegative"
  | Rnd_0 -> "toZero"

let canonicalize_rnd rnd = { rnd with
  (* TODO: Resetting the coefficient is not ideal *)
  coefficient = 1.0;
  fp_type = if value_type_to_string rnd.fp_type = "real" then real_type
            else rnd.fp_type;
}

let props_of_rnd prev_rnd rnd =
  let prec' = precision_of_rounding prev_rnd in
  let prec = precision_of_rounding rnd in
  let rm' = rm_of_rounding prev_rnd in
  let rm = rm_of_rounding rnd in
  [if prec <> prec' then ":precision " ^ prec else "";
   if rm <> rm' then ":round " ^ rm else ""]
    |> List.filter (fun x -> x <> "")
    |> String.concat " "

let no_rnd = create_rounding 0 "ne" 1.0

let real_rnd = { no_rnd with fp_type = mk_value_type max_int }

(* Rounding operations in the transformed expression correspond
   to FPBench context annotations.
   no_rnd corresponds to cast *)
(* This function is not used because a direct translation to FPCore
   yields slighly less verbose results (the context annotations
   do not contain properties which are not changing) *)
let rec transform_expr types ctx_rnd expr =
  let rnd', expr' =
    match expr with
    | Rounding (rnd, arg) -> rnd, arg
    | _ -> real_rnd, expr in
  let rnd = canonicalize_rnd rnd' in
  let convert expr =
    match expr with
    | Const _ -> expr
    | Var v ->
      let ty = types v in
      if ty <> rnd.fp_type then Rounding (no_rnd, expr) else expr
    | Rounding (rnd', arg) ->
      if canonicalize_rnd rnd' <> rnd then
        Rounding (no_rnd, transform_expr types rnd expr)
      else
        transform_expr types rnd expr
    | U_op (op, arg) -> U_op (op, transform_expr types rnd expr)
    | Bin_op (op, arg1, arg2) ->
      Bin_op (op, transform_expr types rnd arg1, transform_expr types rnd arg2)
    | Gen_op (op, args) ->
      Gen_op (op, List.map (transform_expr types rnd) args)
  in
  if rnd <> ctx_rnd then
    Rounding (rnd, convert expr')
  else
    convert expr'

let rec export_expr types ctx_rnd fmt expr =
  let rnd', expr' =
    match expr with
    | Rounding (rnd, arg) -> rnd, arg
    | _ -> real_rnd, expr in
  let rnd = canonicalize_rnd rnd' in
  let print = export_expr types rnd in
  let convert_expr fmt expr =
    match expr with
    | Var v ->
      let ty = types v in
      if ty <> rnd.fp_type then
        fprintf fmt "(cast %s)" v
      else
        fprintf fmt "%s" v
    | Const (Const.Rat n) -> fprintf fmt "%s" (Num.string_of_num n)
    | Const (Const.Interval v) ->
      let a = Num.string_of_num (More_num.num_of_float v.low) and
          b = Num.string_of_num (More_num.num_of_float v.high) in
      fprintf fmt "(interval %s %s)" a b
    | Rounding (rnd', arg) ->
      if canonicalize_rnd rnd' <> rnd then
        fprintf fmt "(cast %a)" print expr
      else
        export_expr types rnd fmt expr
    | U_op (op, arg) -> begin
        match op with
        | Op_neg -> fprintf fmt "(- %a)" print arg
        | Op_abs -> fprintf fmt "(fabs %a)" print arg
        | Op_inv -> fprintf fmt "(/ 1 %a)" print arg
        | Op_sqrt -> fprintf fmt "(sqrt %a)" print arg
        | Op_exp -> fprintf fmt "(exp %a)" print arg
        | Op_log -> fprintf fmt "(log %a)" print arg
        | Op_sin -> fprintf fmt "(sin %a)" print arg
        | Op_cos -> fprintf fmt "(cos %a)" print arg
        | Op_tan -> fprintf fmt "(tan %a)" print arg
        | Op_asin -> fprintf fmt "(asin %a)" print arg
        | Op_acos -> fprintf fmt "(acos %a)" print arg
        | Op_atan -> fprintf fmt "(atan %a)" print arg
        | Op_sinh -> fprintf fmt "(sinh %a)" print arg
        | Op_cosh -> fprintf fmt "(cosh %a)" print arg
        | Op_tanh -> fprintf fmt "(tanh %a)" print arg                       
        | Op_asinh -> fprintf fmt "(asinh %a)" print arg
        | Op_acosh -> fprintf fmt "(acosh %a)" print arg
        | Op_atanh -> fprintf fmt "(atanh %a)" print arg
        (* Non-standard FPCore extension: p2 *)
        | Op_floor_power2 -> fprintf fmt "(p2 %a)" print arg
      end
    | Bin_op (op, arg1, arg2) -> begin
        match op with
        | Op_min -> fprintf fmt "@[<6>(fmin %a@ %a)@]" print arg1 print arg2
        | Op_max -> fprintf fmt "@[<6>(fmax %a@ %a)@]" print arg1 print arg2
        | Op_add -> fprintf fmt "@[<3>(+ %a@ %a)@]" print arg1 print arg2
        | Op_sub -> fprintf fmt "@[<3>(- %a@ %a)@]" print arg1 print arg2
        | Op_mul -> fprintf fmt "@[<3>(* %a@ %a)@]" print arg1 print arg2
        | Op_div -> fprintf fmt "@[<3>(/ %a@ %a)@]" print arg1 print arg2
        | Op_nat_pow -> fprintf fmt "@[<5>(pow %a@ %a)@]" print arg1 print arg2
        | _ -> failwith ("FPCore: unknown binary operation: " ^ bin_op_name op)
      end
    | Gen_op (op, args) -> begin
        match (op, args) with
        | Op_fma, [a1; a2; a3] ->
           fprintf fmt "(fma %a %a %a)" print a1 print a2 print a3
        | _ -> failwith ("FPCore: unknown general operation: " ^ gen_op_name op)
      end
  in
  if rnd <> ctx_rnd then
    fprintf fmt "(! %s %a)" (props_of_rnd ctx_rnd rnd) convert_expr expr'
  else
    convert_expr fmt expr'

let var_bounds_to_pre fmt task =
  let print_bounds v =
    let a, b = variable_num_interval task v in
    fprintf fmt "(<= %s %s %s)" (Num.string_of_num a) v (Num.string_of_num b) in
  Lib.print_list print_bounds (sep fmt " ") (all_variables task)

let constraints_to_pre fmt task =
  let formulas = List.map snd task.constraints in
  Lib.print_list (print_formula fmt) (sep fmt " ") formulas

let select_precision var_types =
  let h = Hashtbl.create 10 in
  var_types |> List.iter (fun x ->
    let v = try Hashtbl.find h x with Not_found -> 0 in
    Hashtbl.replace h x (v + 1)
  );
  let ty, _ =
    Hashtbl.fold (fun ty v ((_, v_max) as r) ->
      if v >= v_max then (ty, v) else r)
      h (real_type, 0) in
  if ty = real_type then 
    real_rnd
  else
    create_rounding (type_size ty) "ne" 1.0

let rec remove_no_rnd expr =
  match expr with
  | Var _ | Const _ -> expr
  | Rounding (rnd, a) when is_no_rnd rnd -> remove_no_rnd a
  | Rounding (rnd, a) -> Rounding (rnd, remove_no_rnd a)
  | U_op (op, a) -> U_op (op, remove_no_rnd a)
  | Bin_op (op, a, b) -> Bin_op (op, remove_no_rnd a, remove_no_rnd b)
  | Gen_op (op, args) -> Gen_op (op, List.map remove_no_rnd args)

let generate_fpcore fmt task =
  let var_names = all_variables task in
  let var_types = List.map (variable_type task) var_names in
  let base_rnd = select_precision var_types in
  let print_arg fmt var =
    let ty = variable_type task var in
    if ty <> base_rnd.fp_type then
      fprintf fmt "(! :precision %s %s)" (precision_of_value_type ty) var
    else
      fprintf fmt "%s" var in
  fprintf fmt "(FPCore (%a)@."
    (fun fmt -> Lib.print_list (print_arg fmt) (sep fmt " ")) var_names;
  fprintf fmt "  :name \"%s\"@." (String.escaped task.name);
  fprintf fmt "  :description \"Generated by FPTaylor\"@.";
  fprintf fmt "  :precision %s@." (precision_of_rounding base_rnd);
  fprintf fmt "  :round %s@." (rm_of_rounding base_rnd);
  if task.constraints = [] then
    if all_variables task <> [] then
      fprintf fmt "  :pre (and %a)@." var_bounds_to_pre task
    else ()
  else
    fprintf fmt "  :pre (and %a %a)@."
      var_bounds_to_pre task constraints_to_pre task;
  (* let expr = transform_expr (variable_type task) base_rnd task.expression in *)
  (* fprintf fmt "  %a)@.@." (Out.print_fmt ~margin:max_int) expr *)
  let expr = remove_no_rnd task.expression in
  fprintf fmt "  %a)@.@." (export_expr (variable_type task) base_rnd) expr
