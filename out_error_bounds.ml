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
  mutable tmp_index : int;
  mutable tmp_max_index : int;
  mutable subexprs : (expr * string) list;
  mutable subexprs_names : (expr * string) list;
}

let mk_env vars = {
  vars = vars;
  const_index = 0;
  constants = [];
  tmp_index = 0;
  tmp_max_index = 0;
  subexprs = [];
  subexprs_names = [];
}

let clear_exprs env =
  env.tmp_index <- 0;
  env.subexprs <- [];
  env.subexprs_names <- []

let get_expr_name env ?(suffix = "") expr =
  try Lib.assoc_eq eq_expr expr env.subexprs, true
  with Not_found ->
    let name, flag =
      match expr with
      | Const c when Const.is_rat c ->
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
            env.tmp_index <- env.tmp_index + 1;
            env.tmp_max_index <- max env.tmp_max_index env.tmp_index;
            let tmp_name = sprintf "t_%d%s" env.tmp_index suffix in
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
            | Op_my_sin -> fprintf fmt "  mpfr_set(%s, %s, MPFR_RNDN);@." name arg_name
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

let translate_mpfi env =
  let rec translate fmt expr =
    let name, found_flag = get_expr_name env expr ~suffix:"" in
    if found_flag then name
    else
      let () =
        match expr with
        | Const c ->
          let x = Const.to_interval c in
          fprintf fmt "  mpfi_interv_d(%s, %.20e, %.20e);@." 
            name x.Interval.low x.Interval.high
        | U_op (op, arg) -> begin
            let arg_name = translate fmt arg in
            match op with
            | Op_neg -> fprintf fmt "  mpfi_neg(%s, %s);@." name arg_name
            | Op_abs -> fprintf fmt "  mpfi_abs(%s, %s);@." name arg_name
            | Op_inv -> fprintf fmt "  mpfi_inv(%s, %s);@." name arg_name
            | Op_sqrt -> fprintf fmt "  mpfi_sqrt(%s, %s);@." name arg_name
            | Op_exp -> fprintf fmt "  mpfi_exp(%s, %s);@." name arg_name
            | Op_log -> fprintf fmt "  mpfi_log(%s, %s);@." name arg_name
            | Op_sin -> fprintf fmt "  mpfi_sin(%s, %s);@." name arg_name
            | Op_my_sin -> fprintf fmt "  mpfi_set(%s, %s);@." name arg_name
            | Op_cos -> fprintf fmt "  mpfi_cos(%s, %s);@." name arg_name
            | Op_floor_power2 -> fprintf fmt "  mpfi_floor_power2(%s, %s);@." name arg_name
            | _ -> failwith ("translate_mpfi: unsupported unary operation: " ^ u_op_name op)
          end
        | Bin_op (op, arg1, arg2) -> begin
            let a1 = translate fmt arg1 in
            let a2 = translate fmt arg2 in
            match op with
            | Op_add -> fprintf fmt "  mpfi_add(%s, %s, %s);@." name a1 a2
            | Op_sub -> fprintf fmt "  mpfi_sub(%s, %s, %s);@." name a1 a2
            | Op_mul -> fprintf fmt "  mpfi_mul(%s, %s, %s);@." name a1 a2
            | Op_div -> fprintf fmt "  mpfi_div(%s, %s, %s);@." name a1 a2
            | Op_sub2 -> fprintf fmt "  mpfi_sub2(%s, %s, %s);@." name a1 a2
            | _ -> failwith ("translate_mpfi: unsupported binary operation: " ^ bin_op_name op)
          end
        | Gen_op (Op_ulp, [Const p; Const e; arg]) ->
          fprintf fmt "  mpfi_goldberg_ulp(%s, %d, %d, %s);@."
            name (Const.to_int p) (Const.to_int e) (translate fmt arg)
        | _ -> failwith ("translate_mpfi: unsupported operation") in
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
            | Op_my_sin -> fprintf fmt "  double %s = %s;@." name arg_name
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
            | Op_my_sin -> fprintf fmt "  float %s = %s;@." name arg_name
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

let print_init_functions env 
    ?(double = false) ?(single = false) ?(mpfi = false) fmt =
  let mp_type, mp_prefix =
    if mpfi then "mpfi_t", "mpfi" else "mpfr_t", "mpfr" in
  let c_names_mp = List.map (fun (_, i) -> sprintf "c_%d" i) env.constants in
  let c_names_double = List.map (fun (_, i) -> sprintf "c_%dd" i) env.constants in
  let c_names_single = List.map (fun (_, i) -> sprintf "c_%df" i) env.constants in
  let tmp_names = Lib.init_list env.tmp_max_index (fun i -> sprintf "t_%d" (i + 1)) in
  let tmp_vars_flag = env.tmp_max_index > 0 in
  let constants_flag = List.length env.constants > 0 in
  if tmp_vars_flag then
    fprintf fmt "static %s %a;@." mp_type (print_list ", ") tmp_names;
  if constants_flag then begin
    fprintf fmt "static %s %a;@." mp_type (print_list ", ") c_names_mp;
    if double then
      fprintf fmt "static double %a;@." (print_list ", ") c_names_double;
    if single then
      fprintf fmt "static float %a;@." (print_list ", ") c_names_single;
  end;
  pp_print_newline fmt ();
  fprintf fmt "void f_init()@.{@.";
  if tmp_vars_flag then
    List.iter (fun v -> fprintf fmt "  %s_init(%s);@." mp_prefix v)
              tmp_names;
  if constants_flag then
    List.iter (fun c -> fprintf fmt "  %s_init(%s);@." mp_prefix c)
              c_names_mp;
  List.iter
    (fun (n, i) -> 
      let f_const = if single then sprintf "&c_%df" i else "NULL" in
      let d_const = if double then sprintf "&c_%dd" i else "NULL" in
        fprintf fmt "  init_constants(\"%s\", MPFR_RNDN, %s, %s, c_%d);@."
          (Num.string_of_num n) f_const d_const i)
    env.constants;
  fprintf fmt "}@.";
  pp_print_newline fmt ();
  fprintf fmt "void f_clear()@.{@.";
  if tmp_vars_flag then
    List.iter (fun v -> fprintf fmt "  %s_clear(%s);@." mp_prefix v)
              tmp_names;
  if constants_flag then
    List.iter (fun c -> fprintf fmt "  %s_clear(%s);@." mp_prefix c)
              c_names_mp;
  fprintf fmt "}@."

let print_mp_f env fmt ?(mpfi = false) ?(index = 1) expr =
  clear_exprs env;
  env.subexprs_names <- [expr, "r_op"];
  let mp_prefix = if mpfi then "mpfi" else "mpfr" in
  let args = List.map (fun (_, name) -> mp_prefix ^ "_srcptr " ^ name) env.vars in
  let body, result_name =
    Lib.write_to_string_result 
      (if mpfi then (translate_mpfi env) else (translate_mpfr env))
      expr in
  let f_name = "f_high" ^ (if index <= 1 then "" else string_of_int index) in
  fprintf fmt "void %s(%s r_op, %a)@.{@." f_name (mp_prefix ^ "_ptr") (print_list ", ") args;
  fprintf fmt "%s" body;
  if result_name <> "r_op" then begin
    if mpfi then 
      fprintf fmt "  mpfi_set(r_op, %s);@." result_name
    else
      fprintf fmt "  mpfr_set(r_op, %s, MPFR_RNDN);@." result_name
  end;
  fprintf fmt "}@."

let print_double_f env fmt expr =
  clear_exprs env;
  let args = List.map (fun (_, name) -> "double " ^ name) env.vars in
  let body, result_name =
    Lib.write_to_string_result (translate_double env) expr in
  fprintf fmt "double f_64(%a)@.{@." (print_list ", ") args;
  fprintf fmt "%s@.  return %s;@.}@." body result_name

let print_single_f env fmt expr =
  clear_exprs env;
  let args = List.map (fun (_, name) -> "float " ^ name) env.vars in
  let body, result_name =
    Lib.write_to_string_result (translate_single env) expr in
  fprintf fmt "float f_32(%a)@.{@." (print_list ", ") args;
  fprintf fmt "%s@.  return %s;@.}@." body result_name

let print_init_f_and_mps env fmt 
      ?(double = false) ?(single = false) ?(mpfi = false) exprs =
  let mps = List.mapi 
              (fun i e -> 
                Lib.write_to_string (print_mp_f env ~mpfi:mpfi ~index:(i + 1)) e) exprs in
  print_init_functions env fmt ~double:double ~single:single ~mpfi:mpfi;
  pp_print_newline fmt ();
  List.iter (fprintf fmt "%s@.") mps

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
  fprintf fmt "#ifdef USE_MPFI@.";
  fprintf fmt "@.#include \"search_mpfi.h\"@.";
  pp_print_newline fmt ();
  print_init_f_and_mps env fmt ~double:true ~single:true ~mpfi:true [expr];
  fprintf fmt "@.#else@.";
  fprintf fmt "@.#include \"search_mpfr.h\"@.";
  pp_print_newline fmt ();
  print_init_f_and_mps env fmt ~double:true ~single:true ~mpfi:false [expr];
  fprintf fmt "@.#endif@.";
  pp_print_newline fmt ();
  let low_str = List.map (fun b -> sprintf "%.20e" b.Interval.low) var_bounds in
  let high_str = List.map (fun b -> sprintf "%.20e" b.Interval.high) var_bounds in
  fprintf fmt "const double low[] = {%a};@." (print_list ", ") low_str;
  fprintf fmt "const double high[] = {%a};@." (print_list ", ") high_str;
  fprintf fmt "const char *f_name = \"%s\";@." task.name;
  pp_print_newline fmt ();
  print_double_f env fmt expr;
  pp_print_newline fmt ();
  print_single_f env fmt expr

let generate_data_functions fmt task named_exprs =
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
  fprintf fmt "#include \"data_mpfi.h\"@.";
  fprintf fmt "#include \"func.h\"@.";
  pp_print_newline fmt ();
  let expr_names, exprs = Lib.unzip named_exprs in
  print_init_f_and_mps env fmt ~mpfi:true exprs;
  pp_print_newline fmt ();
  let low_str = List.map (fun b -> sprintf "%.20e" b.Interval.low) var_bounds in
  let high_str = List.map (fun b -> sprintf "%.20e" b.Interval.high) var_bounds in
  fprintf fmt "const double low[] = {%a};@." (print_list ", ") low_str;
  fprintf fmt "const double high[] = {%a};@." (print_list ", ") high_str;
  let expr_names = List.map (sprintf "\"%s\"") expr_names in
  fprintf fmt "const char *f_names[] = {%a};@." (print_list ", ") expr_names;
  let n = List.length exprs in
  let f_names = 
    Lib.init_list n (fun i -> "f_high" ^ (if i = 0 then "" else string_of_int (i + 1))) in
  fprintf fmt "const int n_funcs = %d;@." (List.length f_names);
  fprintf fmt "const F_HIGH funcs[] = {%a};@." (print_list ", ") f_names;
  fprintf fmt "const char *expression_string = \"%s\";@."
    (ExprOut.Info.print_str (remove_rnd task.expression))
