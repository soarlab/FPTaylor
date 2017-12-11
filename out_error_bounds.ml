(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT license           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* C output for the ErrorBounds tool                                          *)
(* -------------------------------------------------------------------------- *)

open Expr
open Task
open Format

let print_list sep fmt =
  Lib.print_list (pp_print_string fmt) (fun () -> pp_print_string fmt sep)

type env = {
  vars : (string * string) list;
  mutable const_index : int;
  mutable constants : (Num.num * int) list;
  mutable expr_index : int;
  mutable tmp_vars : string list;
  mutable subexprs : (expr * string) list;
  mutable subexprs_names : (expr * string) list;
}

let mk_env vars = {
  vars = vars;
  const_index = 0;
  constants = [];
  expr_index = 0;
  tmp_vars = [];
  subexprs = [];
  subexprs_names = [];
}

let clear_exprs env =
  env.expr_index <- 0;
  env.tmp_vars <- [];
  env.subexprs <- [];
  env.subexprs_names <- []

let get_expr_name env ?(suffix = "") expr =
  try Lib.assoc_eq eq_expr expr env.subexprs, true
  with Not_found ->
    let name, flag =
      match expr with
      | Const c ->
        let index =
          let n = Const.to_num c in
          try Lib.assoc_eq Num.eq_num n env.constants
          with Not_found ->
            env.const_index <- env.const_index + 1;
            env.constants <- (n, env.const_index) :: env.constants;
            env.const_index in
        sprintf "c_%d%s" index suffix, true
      | Var v -> Lib.assoc v env.vars, true
      | _ -> begin
          try Lib.assoc_eq eq_expr expr env.subexprs_names, false
          with Not_found ->
            env.expr_index <- env.expr_index + 1;
            let tmp_name = sprintf "t_%d%s" env.expr_index suffix in
            env.tmp_vars <- tmp_name :: env.tmp_vars;
            tmp_name, false 
        end in
    env.subexprs <- (expr, name) :: env.subexprs;
    name, flag

let translate_mpfr env =
  let rec translate fmt expr =
    let name, found_flag = get_expr_name env expr ~suffix:"" in
    if found_flag then name
    else
      let () =
        match expr with
        | U_op (op, arg) -> begin
            let arg_name = translate fmt arg in
            match op with
            | Op_neg -> fprintf fmt "  mpfr_neg(%s, %s, MPFR_RNDN);@." name arg_name
            | Op_abs -> fprintf fmt "  mpfr_abs(%s, %s, MPFR_RNDN);@." name arg_name
            | Op_inv -> fprintf fmt "  mpfr_d_div(%s, 1.0, %s, MPFR_RNDN);@." name arg_name
            | Op_sqrt -> fprintf fmt "  mpfr_sqrt(%s, %s, MPFR_RNDN);@." name arg_name
            | Op_exp -> fprintf fmt "  mpfr_exp(%s, %s, MPFR_RNDN);@." name arg_name
            | Op_log -> fprintf fmt "  mpfr_log(%s, %s, MPFR_RNDN);@." name arg_name
            | Op_sin -> fprintf fmt "  mpfr_sin(%s, %s, MPFR_RNDN);@." name arg_name
            | Op_cos -> fprintf fmt "  mpfr_cos(%s, %s, MPFR_RNDN);@." name arg_name
            | _ -> failwith ("translate_mpfr: unsupported unary operation: " ^ u_op_name op)
          end
        | Bin_op (op, arg1, arg2) -> begin
            let a1 = translate fmt arg1 in
            let a2 = translate fmt arg2 in
            match op with
            | Op_min -> fprintf fmt "  mpfr_min(%s, %s, %s, MPFR_RNDN);@." name a1 a2
            | Op_max -> fprintf fmt "  mpfr_max(%s, %s, %s, MPFR_RNDN);@." name a1 a2
            | Op_add -> fprintf fmt "  mpfr_add(%s, %s, %s, MPFR_RNDN);@." name a1 a2
            | Op_sub -> fprintf fmt "  mpfr_sub(%s, %s, %s, MPFR_RNDN);@." name a1 a2
            | Op_mul -> fprintf fmt "  mpfr_mul(%s, %s, %s, MPFR_RNDN);@." name a1 a2
            | Op_div -> fprintf fmt "  mpfr_div(%s, %s, %s, MPFR_RNDN);@." name a1 a2
            | _ -> failwith ("translate_mpfr: unsupported binary operation: " ^ bin_op_name op)
          end
        | _ -> failwith ("translate_mpfr: unsupported operation") in
      name 
  in
  translate

let translate_double env =
  let rec translate fmt expr =
    let name, found_flag = get_expr_name env expr ~suffix:"d" in
    if found_flag then name
    else
      let () =
        match expr with
        | U_op (op, arg) -> begin
            let arg_name = translate fmt arg in
            match op with
            | Op_neg -> fprintf fmt "  double %s = -%s;@." name arg_name
            | Op_abs -> fprintf fmt "  double %s = fabs(%s);@." name arg_name
            | Op_inv -> fprintf fmt "  double %s = 1.0 / %s;@." name arg_name
            | Op_sqrt -> fprintf fmt "  double %s = sqrt(%s);@." name arg_name
            | Op_exp -> fprintf fmt "  double %s = exp(%s);@." name arg_name
            | Op_log -> fprintf fmt "  double %s = log(%s);@." name arg_name
            | Op_sin -> fprintf fmt "  double %s = sin(%s);@." name arg_name
            | Op_cos -> fprintf fmt "  double %s = cos(%s);@." name arg_name
            | _ -> failwith ("translate_double: unsupported unary operation: " ^ u_op_name op)
          end
        | Bin_op (op, arg1, arg2) -> begin
            let a1 = translate fmt arg1 in
            let a2 = translate fmt arg2 in
            match op with
            | Op_min -> fprintf fmt "  double %s = fmin(%s, %s);@." name a1 a2
            | Op_max -> fprintf fmt "  double %s = fmax(%s, %s);@." name a1 a2
            | Op_add -> fprintf fmt "  double %s = %s + %s;@." name a1 a2
            | Op_sub -> fprintf fmt "  double %s = %s - %s;@." name a1 a2
            | Op_mul -> fprintf fmt "  double %s = %s * %s;@." name a1 a2
            | Op_div -> fprintf fmt "  double %s = %s / %s;@." name a1 a2
            | _ -> failwith ("translate_double: unsupported binary operation: " ^ bin_op_name op)
          end
        | _ -> failwith ("translate_double: unsupported operation") in
      name 
  in
  translate

let translate_single env =
  let rec translate fmt expr =
    let name, found_flag = get_expr_name env expr ~suffix:"f" in
    if found_flag then name
    else
      let () =
        match expr with
        | U_op (op, arg) -> begin
            let arg_name = translate fmt arg in
            match op with
            | Op_neg -> fprintf fmt "  float %s = -%s;@." name arg_name
            | Op_abs -> fprintf fmt "  float %s = fabsf(%s);@." name arg_name
            | Op_inv -> fprintf fmt "  float %s = 1.0f / %s;@." name arg_name
            | Op_sqrt -> fprintf fmt "  float %s = sqrtf(%s);@." name arg_name
            | Op_exp -> fprintf fmt "  float %s = expf(%s);@." name arg_name
            | Op_log -> fprintf fmt "  float %s = logf(%s);@." name arg_name
            | Op_sin -> fprintf fmt "  float %s = sinf(%s);@." name arg_name
            | Op_cos -> fprintf fmt "  float %s = cosf(%s);@." name arg_name
            | _ -> failwith ("translate_single: unsupported unary operation: " ^ u_op_name op)
          end
        | Bin_op (op, arg1, arg2) -> begin
            let a1 = translate fmt arg1 in
            let a2 = translate fmt arg2 in
            match op with
            | Op_min -> fprintf fmt "  float %s = fminf(%s, %s);@." name a1 a2
            | Op_max -> fprintf fmt "  float %s = fmaxf(%s, %s);@." name a1 a2
            | Op_add -> fprintf fmt "  float %s = %s + %s;@." name a1 a2
            | Op_sub -> fprintf fmt "  float %s = %s - %s;@." name a1 a2
            | Op_mul -> fprintf fmt "  float %s = %s * %s;@." name a1 a2
            | Op_div -> fprintf fmt "  float %s = %s / %s;@." name a1 a2
            | _ -> failwith ("translate_single: unsupported binary operation: " ^ bin_op_name op)
          end
        | _ -> failwith ("translate_single: unsupported operation") in
      name 
  in
  translate

let remove_rnd expr =
  let rec remove expr =
    match expr with
    | Const c -> expr
    | Var v -> expr
    | U_op (op, arg) -> U_op (op, remove arg)
    | Bin_op (op, arg1, arg2) -> Bin_op (op, remove arg1, remove arg2)
    | Gen_op (op, args) -> Gen_op (op, List.map remove args)
    | Rounding (rnd, arg) -> remove arg in
  remove expr

let print_mpfr_and_init_f fmt env expr =
  clear_exprs env;
  env.subexprs_names <- [expr, "r_op"];
  let args = List.map (fun (_, name) -> "mpfr_t " ^ name) env.vars in
  let body, result_name =
    Lib.write_to_string_result (translate_mpfr env) expr in
  let c_names_mpfr = List.map (fun (_, i) -> sprintf "c_%d" i) env.constants in
  let c_names_double = List.map (fun (_, i) -> sprintf "c_%dd" i) env.constants in
  let c_names_single = List.map (fun (_, i) -> sprintf "c_%df" i) env.constants in
  let tmp_vars_flag = List.length env.tmp_vars > 0 in
  let constants_flag = List.length env.constants > 0 in
  if tmp_vars_flag then
    fprintf fmt "static mpfr_t %a;@." (print_list ", ") env.tmp_vars;
  if constants_flag then begin
    fprintf fmt "static mpfr_t %a;@." (print_list ", ") c_names_mpfr;
    fprintf fmt "static double %a;@." (print_list ", ") c_names_double;
    fprintf fmt "static float %a;@." (print_list ", ") c_names_single;
  end;
  pp_print_newline fmt ();
  fprintf fmt "void f_init()@.{@.";
  if tmp_vars_flag then
    fprintf fmt "  mpfr_inits(%a, NULL);@." (print_list ", ") env.tmp_vars;
  if constants_flag then
    fprintf fmt "  mpfr_inits(%a, NULL);@." (print_list ", ") c_names_mpfr;
  List.iter 
    (fun (n, i) -> 
       fprintf fmt "  init_constants(\"%s\", MPFR_RNDN, &c_%df, &c_%dd, c_%d);@."
         (Num.string_of_num n) i i i) 
    env.constants;
  fprintf fmt "}@.";
  pp_print_newline fmt ();
  fprintf fmt "void f_clear()@.{@.";
  if tmp_vars_flag then
    fprintf fmt "  mpfr_clears(%a, NULL);@." (print_list ", ") env.tmp_vars;
  if constants_flag then
    fprintf fmt "  mpfr_clears(%a, NULL);@." (print_list ", ") c_names_mpfr;
  fprintf fmt "}@.";
  pp_print_newline fmt ();
  fprintf fmt "void f_mpfr(mpfr_t r_op, %a)@.{@." (print_list ", ") args;
  fprintf fmt "%s" body;
  if result_name <> "r_op" then
    fprintf fmt "  mpfr_set(r_op, %s, MPFR_RNDN);@." result_name;
  fprintf fmt "}@."

let print_double_f fmt env expr =
  clear_exprs env;
  let args = List.map (fun (_, name) -> "double " ^ name) env.vars in
  let body, result_name =
    Lib.write_to_string_result (translate_double env) expr in
  fprintf fmt "double f_64(%a)@.{@." (print_list ", ") args;
  fprintf fmt "%s@.  return %s;@.}@." body result_name

let print_single_f fmt env expr =
  clear_exprs env;
  let args = List.map (fun (_, name) -> "float " ^ name) env.vars in
  let body, result_name =
    Lib.write_to_string_result (translate_single env) expr in
  fprintf fmt "float f_32(%a)@.{@." (print_list ", ") args;
  fprintf fmt "%s@.  return %s;@.}@." body result_name

let generate_error_bounds fmt task =
  let task_vars, var_bounds = 
    let vars = all_active_variables task in
    let bounds = List.map (variable_interval task) vars in
    match vars with
    | [] -> ["unused"],
            let names = all_variables task in
            if names <> [] then
              [variable_interval task (List.hd names)]
            else
              [{Interval.low = 1.; Interval.high = 2.}]
    | _ -> vars, bounds in
  let var_names = List.map (fun s -> "v_" ^ ExprOut.fix_name s) task_vars in
  let env = mk_env (Lib.zip task_vars var_names) in
  let expr = remove_rnd task.expression in
  fprintf fmt "#include \"search_mpfr.h\"@.";
  fprintf fmt "#include \"search_mpfr_utils.h\"@.";
  pp_print_newline fmt ();
  let low_str = List.map (fun b -> sprintf "%.20e" b.Interval.low) var_bounds in
  let high_str = List.map (fun b -> sprintf "%.20e" b.Interval.high) var_bounds in
  fprintf fmt "double low[] = {%a};@." (print_list ", ") low_str;
  fprintf fmt "double high[] = {%a};@." (print_list ", ") high_str;
  pp_print_newline fmt ();
  print_mpfr_and_init_f fmt env expr;
  pp_print_newline fmt ();
  print_double_f fmt env expr;
  pp_print_newline fmt ();
  print_single_f fmt env expr
