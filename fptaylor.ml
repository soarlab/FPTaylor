(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT license           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* Main FPTaylor functions                                                    *)
(* -------------------------------------------------------------------------- *)

open Interval
open Expr
open Task
open Taylor_form

type result = {
  task : task;
  real_bounds : interval;
  abs_error_model : expr option;
  rel_error_model : expr option;
  ulp_error_model : expr option;
  (* Lower bounds of error intervals represent lower bounds
     returned by a global optimization procedure.
     low = neg_infinity if a lower bound is not returned. *)
  abs_error_approx : interval option;
  abs_error_exact : interval option;
  rel_error_approx : interval option;
  rel_error_exact : interval option;
  ulp_error_approx : interval option;
  ulp_error_exact : interval option;
  elapsed_time : float;
}

let mk_result task = {
  task = task;
  real_bounds = {low = neg_infinity; high = infinity};
  abs_error_model = None;
  rel_error_model = None;
  ulp_error_model = None;
  abs_error_approx = None;
  abs_error_exact = None;
  rel_error_approx = None;
  rel_error_exact = None;
  ulp_error_approx = None;
  ulp_error_exact = None;
  elapsed_time = 0.0;
}

let open_file, close_file, close_all, get_file_formatter =
  let files = Hashtbl.create 5 in
  let open_file id fname =
    if Hashtbl.mem files id then
      failwith ("File with the same id is already open: " ^ id)
    else
      let oc = open_out fname in
      let fmt = Format.make_formatter (output_substring oc) (fun () -> flush oc) in
      Hashtbl.add files id (oc, fmt) in
  let close_file id =
    try
      let oc, fmt = Hashtbl.find files id in
      Format.pp_print_flush fmt ();
      close_out oc;
      Hashtbl.remove files id
    with Not_found -> () in
  let close_all () =
    Hashtbl.iter 
      (fun _ (oc, fmt) -> Format.pp_print_flush fmt (); close_out oc) 
      files;
    Hashtbl.clear files in
  let get_fmt id = 
    snd (Hashtbl.find files id) in
  open_file, close_file, close_all, get_fmt

let get_problem_absolute_error result =
  let entire = {low = neg_infinity; high = infinity} in
  let e1 = Lib.option_default ~default:entire result.abs_error_approx and
    e2 = Lib.option_default ~default:entire result.abs_error_exact in
  min e1.high e2.high

let print_result (result : result) =
  let hex = Config.get_bool_option "print-hex-floats" in
  let prec = Config.get_int_option "print-precision" in
  let print_upper_bound width str = function
    | None -> ()
    | Some v ->
      let bound = More_num.string_of_float_hi prec v.high in
      if hex then
        Log.report `Main "%-*s %s (%h)" width str bound v.high
      else
        Log.report `Main "%-*s %s" width str bound in
  let print_lower_bound width str = function
    | None -> ()
    | Some v ->
      let bound = More_num.string_of_float_lo prec v.low in
      if v.low > neg_infinity then
        let subopt = v.high -. v.low in
        if hex then
          Log.report `Main "%-*s %s (%h) (suboptimality = %.1f%%)"
            width str bound v.low (subopt /. v.high *. 100.)
        else
          Log.report `Main "%-*s %s (suboptimality = %.1f%%)"
            width str bound (subopt /. v.high *. 100.)
      else if hex then
        Log.report `Main "%-*s %s (%h)" width str bound v.low
      else
        Log.report `Main "%-*s %s" width str bound in
  let string_of_interval r =
    let lo = More_num.string_of_float_lo prec r.low in
    let hi = More_num.string_of_float_hi prec r.high in
    Printf.sprintf "[%s, %s]" lo hi in
  let print_bounds r =
    if r.real_bounds.low > neg_infinity || r.real_bounds.high < infinity then
      begin
        let err = get_problem_absolute_error r in
        assert (err >= 0.);
        if err < infinity then
          let bounds = r.real_bounds +$ {low = -.err; high = err} in
          Log.report `Main "Bounds (floating-point): %s" (string_of_interval bounds)
      end in
  let rec max_length strs_and_opts =
    match strs_and_opts with
    | [] -> 0
    | (s, None) :: rest -> max_length rest
    | (s, Some _) :: rest -> max (String.length s) (max_length rest)
  in
  Log.report `Main
    "-------------------------------------------------------------------------------";
  Log.report `Main "Problem: %s\n" result.task.name;
  if Config.get_bool_option "print-opt-lower-bounds" then begin
    let abs_approx_str = "The absolute error model (approximate):" in
    let abs_exact_str = "The absolute error model (exact):" in
    let rel_approx_str = "The relative error model (approximate):" in
    let rel_exact_str = "The relative error model (exact):" in
    let ulp_approx_str = "The ULP error model (approximate):" in
    let ulp_exact_str = "The ULP error model (exact):" in
    let w = max_length [abs_approx_str, result.abs_error_approx;
                        abs_exact_str, result.abs_error_exact;
                        rel_approx_str, result.rel_error_approx;
                        rel_exact_str, result.rel_error_exact;
                        ulp_approx_str, result.ulp_error_approx;
                        ulp_exact_str, result.ulp_error_exact] in
    if w > 0 then
      Log.report `Main "Optimization lower bounds for error models:";
    print_lower_bound w abs_approx_str result.abs_error_approx;
    print_lower_bound w abs_exact_str result.abs_error_exact;
    print_lower_bound w rel_approx_str result.rel_error_approx;
    print_lower_bound w rel_exact_str result.rel_error_exact;
    print_lower_bound w ulp_approx_str result.ulp_error_approx;
    print_lower_bound w ulp_exact_str result.ulp_error_exact;
    Log.report `Main "";
  end;
  Log.report `Main "Bounds (without rounding): %s" (string_of_interval result.real_bounds);
  print_bounds result;
  Log.report `Main "";
  let abs_approx_str = "Absolute error (approximate):" in
  let abs_exact_str = "Absolute error (exact):" in
  let rel_approx_str = "Relative error (approximate):" in
  let rel_exact_str = "Relative error (exact):" in
  let ulp_approx_str = "ULP error (approximate):" in
  let ulp_exact_str = "ULP error (exact):" in
  let w = max_length [abs_approx_str, result.abs_error_approx;
                      abs_exact_str, result.abs_error_exact;
                      rel_approx_str, result.rel_error_approx;
                      rel_exact_str, result.rel_error_exact;
                      ulp_approx_str, result.ulp_error_approx;
                      ulp_exact_str, result.ulp_error_exact] in
  print_upper_bound w abs_approx_str result.abs_error_approx;
  print_upper_bound w abs_exact_str result.abs_error_exact;
  print_upper_bound w rel_approx_str result.rel_error_approx;
  print_upper_bound w rel_exact_str result.rel_error_exact;
  print_upper_bound w ulp_approx_str result.ulp_error_approx;
  print_upper_bound w ulp_exact_str result.ulp_error_exact;
  Log.report `Main "\nElapsed time: %.2f\n" result.elapsed_time

let print_form level f =
  Log.report level "v0 = %s" (ExprOut.Info.print_str f.v0);
  List.iter (fun (e, err) -> 
      Log.report level "%d (%d): exp = %d: %s" 
        err.index err.proof_index err.exp (ExprOut.Info.print_str e)) f.v1;
  Log.report level "\nCorresponding original subexpressions:";
  List.iter (fun (_, err) ->
      let i = err.index in
      if i > 0 then
        let expr = expr_for_index i in
        Log.report level "%d: %s" i (ExprOut.Info.print_str expr)) f.v1

let bound_info bound =
  if bound.low > neg_infinity then
    let subopt = (bound.high -. bound.low) /. bound.high *. 100. in
    Format.sprintf "%e (low = %e, subopt = %.1f%%)"
      bound.high bound.low subopt
  else
    Format.sprintf "%e (low = %e)" bound.high bound.low

let add2_symbolic (e1, exp1) (e2, exp2) =
  (* Swap if exp1 > exp2 *)
  let e1, exp1, e2, exp2 =
    if exp1 <= exp2 then e1, exp1, e2, exp2 else e2, exp2, e1, exp1 in
  if exp1 = 0 then 
    (if exp2 = 0 then (const_0, 0) else (e2, exp2))
  else if exp2 = 0 then 
    (e1, exp1)
  else if exp1 = exp2 then
    (mk_add e1 e2, exp1)
  else
    let eps = Rounding.get_eps (exp1 - exp2) in
    (mk_add (mk_mul (mk_float_const eps) e1) e2, exp2)

let sum_symbolic s = Lib.itlist add2_symbolic s (const_0, 0)

let compute_bound cs (expr, err) =
  let r = Opt.find_max_abs (Opt_common.default_opt_pars ()) cs expr in
  let bound = {low = r.Opt_common.lower_bound; high = r.Opt_common.result} in
  Log.report `Info "%d: exp = %d: %s" err.index err.exp (bound_info bound);
  bound, err.exp 

let rec split_error_terms err_terms =
  match err_terms with
  | [] -> [], []
  | (e, err) :: rest ->
    let es1, es2 = split_error_terms rest in
    if err.index < 0 then
      es1, (e, err) :: es2
    else
      (e, err) :: es1, es2

let sum_err_bounds bounds =
  let high = List.map (fun (v, exp) -> v.high, exp) bounds in
  let low = List.map (fun (v, exp) -> -.v.low, exp) bounds in
  let s_high, exp = sum_high high in
  let s_low, exp' =
    let s, e = sum_high low in
    -.s, e in
  assert (exp = exp');
  let eps = Rounding.get_eps exp in
  eps *.$ {low = s_low; high = s_high}

(* Issue a warning if the second-order error term is too large *)
let error2_warning ?(eps = 1e-2) err1 err2 =
  if abs_float err1 > 0. && abs_float err2 >= eps *. abs_float err1 then begin
    Log.warning "Large second-order error: %e (first-order = %e)" err2 err1;
    Log.warning "Try intermediate-opt = true or \
                 manually split intervals of input variables.";
  end

let absolute_errors task tf =
  Log.report `Important "\nComputing absolute errors";
  let cs = constraints_of_task task in
  let v1, v2 = split_error_terms tf.v1 in
  let bounds2 =
    let bounds2' = List.map (compute_bound cs) v2 in
    List.map (fun (e, exp) -> make_stronger_i e, exp) bounds2' in
  let total2_i = sum_err_bounds bounds2 in
  let err_approx =
    if not (Config.get_bool_option "opt-approx") then None
    else
      begin
        Log.report `Important "\nSolving the approximate optimization problem";
        Log.report `Important "\nAbsolute errors:";
        let bounds1 =
          let bounds1' = List.map (compute_bound cs) v1 in
          List.map (fun (e, exp) -> make_stronger_i e, exp) bounds1' in
        let total1_i = sum_err_bounds bounds1 in
        let total_i = make_stronger_i (total1_i +$ total2_i) in
        let () =
          let all_bounds = List.map (fun (v, _) -> v.high) bounds1
                           @ List.map (fun (v, _) -> v.high) bounds2 in
          let all_indices = List.map (fun (_, err) -> err.proof_index) v1 
                            @ List.map (fun (_, err) -> err.proof_index) v2 in
          Proof.add_opt_approx all_indices all_bounds total_i.high in
        Log.report `Important "total1: %s" (bound_info total1_i);
        Log.report `Important "total2: %s" (bound_info total2_i);
        Log.report `Important "total: %s" (bound_info total_i);
        error2_warning total1_i.high total2_i.high;
        Some total_i
      end
  in
  let err_exact, model_expr =
    if not (Config.get_bool_option "opt-exact") then None, None
    else
      begin
        Log.report `Important "\nSolving the exact optimization problem";
        let abs_exprs = List.map (fun (e, err) -> mk_abs e, err.exp) v1 in
        let full_expr, exp =
          let full_expr', exp = sum_symbolic abs_exprs in
          if Config.get_bool_option "maxima-simplification" then
            Maxima.simplify task full_expr', exp
          else
            full_expr', exp in
        let bound =
          let r = Opt.find_max (Opt_common.default_opt_pars ()) cs full_expr in
          {low = r.Opt_common.lower_bound; high = r.Opt_common.result} in
        let total1_i = Rounding.get_eps exp *.$ bound in
        let total_i = 
          if Config.proof_flag () then begin
            let e' = Rounding.get_eps exp in
            let e = if e' = 0.0 then 1.0 else e' in
            let bound = make_stronger_i (bound +$ total2_i /$. e) in
            let total_i = e *.$ bound in
            Proof.add_opt_exact bound.high exp total_i.high;
            total_i
          end
          else
            total1_i +$ total2_i in
        let model_expr = mk_add (mk_mul (mk_float_const (Rounding.get_eps exp)) full_expr)
                                (mk_float_const total2_i.high) in

        let () = try
          let name = task.Task.name in
            Out_error_bounds.generate_data_functions
              (get_file_formatter "data") task
              [name, model_expr;
               name ^ "-total2", mk_float_const total2_i.high;
               name ^ "-opt-bound", mk_float_const total_i.high]
          with Not_found -> () in

        Log.report `Important "exact bound (exp = %d): %s" exp (bound_info bound);
        Log.report `Important "total2: %s" (bound_info total2_i);
        Log.report `Important "exact total: %s" (bound_info total_i);
        error2_warning total1_i.high total2_i.high;
        Some total_i, Some model_expr
      end
  in
  err_approx, err_exact, model_expr

let relative_errors task tf (f_min, f_max) =
  Log.report `Important "\nComputing relative errors";
  let cs = constraints_of_task task in
  let f_int = {low = f_min; high = f_max} in
  let rel_tol = Config.get_float_option "rel-error-threshold" in
  if (abs_I f_int).low < rel_tol then begin
    Log.warning "\nCannot compute the relative error: \
                 values of the function are close to zero";
    None, None, None
  end
  else
    let v1, v2 = split_error_terms tf.v1 in
    let bounds2 = List.map (compute_bound cs) v2 in
    let total2_i = sum_err_bounds bounds2 in
    let b2_i = total2_i /$ abs_I f_int in
    let err_approx =
      if not (Config.get_bool_option "opt-approx") then None
      else
        begin
          let v1_rel = List.map (fun (e, err) -> mk_div e tf.v0, err) v1 in
          let v1_rel = 
            if Config.get_bool_option "maxima-simplification" then
            List.map (fun (e, err) -> Maxima.simplify task e, err) v1_rel
            else
              v1_rel in
          Log.report `Important "\nSolving the approximate optimization probelm";
          Log.report `Important "\nRelative errors:";
          let bounds1 = List.map (compute_bound cs) v1_rel in
          let total1_i = sum_err_bounds bounds1 in
          let total_i = total1_i +$ b2_i in
          Log.report `Important "rel-total1: %s" (bound_info total1_i);
          Log.report `Important "rel-total2: %s" (bound_info b2_i);
          Log.report `Important "rel-total: %s" (bound_info total_i);
          error2_warning total1_i.high b2_i.high;
          Some total_i          
        end
    in
    let err_exact, model_expr =
      if not (Config.get_bool_option "opt-exact") then None, None
      else
        begin
          Log.report `Important "\nSolving the exact optimization problem";
          let full_expr, exp =
            let abs_exprs = List.map (fun (e, err) -> mk_abs e, err.exp) v1 in
            let sum_expr, exp = sum_symbolic abs_exprs in
            let full_expr' = mk_div sum_expr (mk_abs tf.v0) in
            if Config.get_bool_option "maxima-simplification" then
              Maxima.simplify task full_expr', exp
            else
              full_expr', exp in
          let bound =
            let r = Opt.find_max (Opt_common.default_opt_pars ()) cs full_expr in
            {low = r.Opt_common.lower_bound; high = r.Opt_common.result} in
          let total1_i = Rounding.get_eps exp *.$ bound in
          let total_i = total1_i +$ b2_i in
          let model_expr = mk_add (mk_mul (mk_float_const (Rounding.get_eps exp)) full_expr)
                                  (mk_float_const b2_i.high) in

          let () = try
            let name = task.Task.name in
              Out_error_bounds.generate_data_functions
                (get_file_formatter "data") task
                [name, model_expr;
                 name ^ "-total2", mk_float_const b2_i.high;
                 name ^ "-opt-bound", mk_float_const total_i.high]
            with Not_found -> () in

          Log.report `Important "exact bound-rel (exp = %d): %s" exp (bound_info bound);
          Log.report `Important "total2: %s" (bound_info b2_i);
          Log.report `Important "exact total-rel: %s" (bound_info total_i);
          error2_warning total1_i.high b2_i.high;
          Some total_i, Some model_expr
        end
    in
    err_approx, err_exact, model_expr

let ulp_errors task tf (f_min, f_max) =
  Log.report `Important "\nComputing ULP errors";
  let cs = constraints_of_task task in
  let prec, min_exp =
    let t = Rounding_simpl.get_type (variable_type task) task.expression in
    let p = Rounding.type_precision t in
    if p <= 0 then failwith (Format.sprintf "Bad precision: %d" p);
    p, Rounding.type_min_exp t in
  Log.report `Important "\nprec = %d, e_min = %d" prec min_exp;
  let f_int = Func.goldberg_ulp_I (prec, min_exp) {low = f_min; high = f_max} in
  if (abs_I f_int).low <= 0. then begin
    Log.warning "\nCannot compute the ULP error: \
                 values of the function are close to zero";
    None, None, None
  end
  else
    let v1, v2 = split_error_terms tf.v1 in
    let bounds2 = List.map (compute_bound cs) v2 in
    let total2_i = sum_err_bounds bounds2 in
    let b2_i = total2_i /$ abs_I f_int in
    let err_approx =
      if not (Config.get_bool_option "opt-approx") then None
      else
        begin
          let v1_rel = List.map (fun (e, err) -> mk_div e (mk_ulp (prec, min_exp) tf.v0), err) v1 in
          Log.report `Important "\nSolving the approximate optimization probelm";
          Log.report `Important "\nULP errors:";
          let bounds1 = List.map (compute_bound cs) v1_rel in
          let total1_i = sum_err_bounds bounds1 in
          let total_i = total1_i +$ b2_i in
          Log.report `Important "ulp-total1: %s" (bound_info total1_i);
          Log.report `Important "ulp-total2: %s" (bound_info b2_i);
          Log.report `Important "ulp-total: %s" (bound_info total_i);
          error2_warning total1_i.high b2_i.high;
          Some total_i          
        end
    in
    let err_exact, model_expr =
      if not (Config.get_bool_option "opt-exact") then None, None
      else
        begin
          Log.report `Important "\nSolving the exact optimization problem";
          let full_expr, exp =
            let abs_exprs = List.map (fun (e, err) -> mk_abs e, err.exp) v1 in
            let sum_expr, exp = sum_symbolic abs_exprs in
            let full_expr' = mk_div sum_expr (mk_abs (mk_ulp (prec, min_exp) tf.v0)) in
            full_expr', exp in
          let bound =
            let r = Opt.find_max (Opt_common.default_opt_pars ()) cs full_expr in
            {low = r.Opt_common.lower_bound; high = r.Opt_common.result} in
          let total1_i = Rounding.get_eps exp *.$ bound in
          let total_i = total1_i +$ b2_i in
          let model_expr = mk_add (mk_mul (mk_float_const (Rounding.get_eps exp)) full_expr)
                                  (mk_float_const b2_i.high) in

          let () = try
            let name = task.Task.name in
              Out_error_bounds.generate_data_functions
                (get_file_formatter "data") task
                [name, model_expr;
                 name ^ "-total2", mk_float_const b2_i.high;
                 name ^ "-opt-bound", mk_float_const total_i.high]
            with Not_found -> () in

          Log.report `Important "exact bound-ulp (exp = %d): %s" exp (bound_info bound);
          Log.report `Important "total2: %s" (bound_info b2_i);
          Log.report `Important "exact total-ulp: %s" (bound_info total_i);
          error2_warning total1_i.high b2_i.high;
          Some total_i, Some model_expr
        end
    in
    err_approx, err_exact, model_expr

let errors task tform =
  let cs = constraints_of_task task in
  let f_min, f_max = 
    if Config.get_bool_option "rel-error" ||
       Config.get_bool_option "ulp-error" || 
       Config.get_bool_option "find-bounds" then
      Opt.find_min_max (Opt_common.default_opt_pars ()) cs tform.v0
    else
      neg_infinity, infinity in
  Log.report `Important "bounds: [%e, %e]" f_min f_max;
  let result = { (mk_result task) with real_bounds = {low = f_min; high = f_max} } in
  let result =
    if Config.get_bool_option "opt-approx" || Config.get_bool_option "opt-exact" then
      let abs_approx, abs_exact, abs_model_expr = 
        if Config.get_bool_option "abs-error" then
          absolute_errors task tform
        else
          None, None, None in
      let rel_approx, rel_exact, rel_model_expr = 
        if Config.get_bool_option "rel-error" then
          relative_errors task tform (f_min, f_max)
        else
          None, None, None in
      let ulp_approx, ulp_exact, ulp_model_expr = 
        if Config.get_bool_option "ulp-error" then
          ulp_errors task tform (f_min, f_max)
        else
          None, None, None in
      {result with
       abs_error_model = abs_model_expr;
       rel_error_model = rel_model_expr;
       ulp_error_model = ulp_model_expr;
       abs_error_approx = abs_approx;
       abs_error_exact = abs_exact;
       rel_error_approx = rel_approx;
       rel_error_exact = rel_exact;
       ulp_error_approx = ulp_approx;
       ulp_error_exact = ulp_exact
      }
    else
      result in
  Log.report `Important "";
  result

let safety_check task =
  try
    Rounding_simpl.check_expr (variable_interval task) task.expression
  with Rounding_simpl.Exceptional_operation (e0, str) ->
    let msg =
      Format.sprintf "\nPotential exception detected: %s at:\n%s"
        str (ExprOut.Info.print_str e0) in
    if Config.fail_on_exception () then
      failwith msg
    else
      (Log.warning_str msg; zero_I)

let compute_form task =
  Log.report `Info "\n*************************************";
  Log.report `Info "Taylor form for: %s" (ExprOut.Info.print_str task.expression);
  if Config.proof_flag () then Proof.new_proof task;
  let start = Unix.gettimeofday() in
  let result, tform = 
    try
      let bound0 = safety_check task in
      Log.report `Info "\nConservative bound: %s" (sprintf_I "%f" bound0);
      let e = Rounding_simpl.simplify_rounding (variable_type task) task.expression in
      Log.report `Info "\nSimplified rounding: %s" (ExprOut.Info.print_str e);
      let cs = constraints_of_task task in
      Log.report `Important "Building Taylor forms...";
      let form' = build_form cs e in
      Log.report `Important "Simplifying Taylor forms...";
      let form = simplify_form cs form' in
      Log.report `Important "success";
      let form = 
        if Config.get_bool_option "maxima-simplification" then {
          form_index = form.form_index;
          v0 = Maxima.simplify task form.v0;
          v1 = List.map (fun (e, err) -> (if err.index < 0 then e else Maxima.simplify task e), err) form.v1;
        }
        else
          form in
      print_form `Info form;
      Log.report `Info "";
      let result = errors task form in
      result, form
    with Failure msg ->
      Log.error_str msg;
      mk_result task, dummy_tform
  in
  let stop = Unix.gettimeofday() in
  Log.report `Info "Elapsed time: %.5f" (stop -. start);
  let () = 
    if Config.proof_flag () then
      begin
        let proof_dir = Config.get_string_option "proof-dir" in
        Log.report `Important "Saving a proof certificate for %s (in %s)" result.task.name proof_dir;
        Proof.save_proof proof_dir (result.task.name ^ ".proof")
      end
  in
  { result with elapsed_time = stop -. start }, tform

let approximate_constraint task (name, c) =
  let e = 
    match c with
    | Le (a, b) -> mk_sub a b
    | Lt (a, b) -> mk_sub a b
    | Eq (a, b) -> failwith "approximate_constraint: Eq is not supported" in
  let c_task = {
    Task.name = name;
    expression = e;
    variables = task.variables;
    constraints = [];
  } in
  Log.report `Important "Constraint form";
  let r, tform = compute_form c_task in
  let err = get_problem_absolute_error r in
  Log.report `Important "\n%s error: %e\n" r.task.name err;
  name, Le (tform.v0, mk_float_const err)

let process_task (task : task) =
  Log.report `Main "Processing: %s" task.Task.name;
  let approx_constraints =
    if task.constraints = [] then [] else begin
      Log.report `Important "\n****** Approximating constraints *******\n";
      List.map (approximate_constraint task) task.constraints
    end in
  let () =
    let data_export = Config.get_string_option "export-error-bounds-data" in
    if data_export <> "" then begin
      let fname = Str.global_replace (Str.regexp "{task}") task.Task.name data_export in
      Log.report `Important "Data (ErrorBounds) export: %s" fname;
      open_file "data" fname
    end in
  let () =
    let error_bounds = Config.get_string_option "export-error-bounds" in
    if error_bounds <> "" then begin
      let fname = Str.global_replace (Str.regexp "{task}") task.Task.name error_bounds in
      Log.report `Important "ErrorBounds export: %s" fname;
      open_file "error-bounds" fname;
      let fmt = get_file_formatter "error-bounds" in
      Out_error_bounds.generate_error_bounds fmt task;
      close_file "error-bounds"
    end in
  let result = compute_form { task with constraints = approx_constraints } in
  close_file "racket";
  close_file "data";
  result

let process_input fname =
  Log.report `Main "Loading: %s" fname;
  let date_str =
    let time = Unix.localtime (Unix.time ()) in
    Format.sprintf "%d-%02d-%02d-%02d%02d%02d"
      (time.Unix.tm_year + 1900) (time.Unix.tm_mon + 1) time.Unix.tm_mday
      time.Unix.tm_hour time.Unix.tm_min time.Unix.tm_sec in
  let () =
    let log_dir = Config.get_string_option "log-base-dir" in
    let log_name =
      let base_name = Filename.basename fname in
      let name =
        match Config.get_string_option "log-append-date" with
        | "start" -> date_str ^ "_" ^ base_name
        | "end" -> base_name ^ "_" ^ date_str
        | _ -> base_name in
      name ^ ".log" in
    Log.open_log ~base_dir:log_dir log_name in
  let () =
    let tmp_base_dir = Config.get_string_option "tmp-base-dir" in
    let tmp_dir = if Config.get_bool_option "tmp-date" then
        Filename.concat tmp_base_dir date_str
      else
        tmp_base_dir in
    Lib.set_tmp_dir tmp_dir in
  Config.print_options `Debug;
  let tasks = Parser.parse_file fname in
  Log.report `Debug "|tasks| = %d" (List.length tasks);
  let results =
    if Config.is_option_defined "fpcore-out" then begin
      Log.report `Main "Exporting to the FPCore format";
      let fmt = get_file_formatter "fpcore-out" in
      List.iter (Out_fpcore.generate_fpcore fmt) tasks;
      []
    end 
    else begin
      let results = List.map process_task tasks in
      Log.report `Info "*************************************\n";
      List.iter (fun (r, tf) -> print_result r) results;
      results
    end in
  Log.close ();
  Log.report `Main "";
  results

let validate_options () =
  let open Config in
  let validate_simplification () =
    if get_bool_option "maxima-simplification" && not (Maxima.test_maxima()) then
      begin
        Log.warning "A computer algebra system Maxima is not installed. \
                     Simplifications are disabled. \
                     Go to http://maxima.sourceforge.net/ to install Maxima.";
        set_option "maxima-simplification" "false"
      end
  in
  let validate_proof_record () =
    if get_bool_option "proof-record" then
      if get_bool_option "fp-power2-model" then
        begin
          Log.warning "Proof certificates (proof-record = true) are not implemented for \
                       the improved rounding model (fp-power2-model = true).";
        end
      else if get_bool_option "develop" then
        begin
          Log.warning "Proof certificates (proof-record = true) are not implemented for \
                       some features of the development mode (develop = true).";
        end
  in
  let validate_other () =
    let prec = get_int_option "print-precision" in
    if prec < 1 || prec > 1000 then begin
      Log.warning "Bad print-precision value: %d" prec;
      set_option "print-precision" "7"
    end
  in
  begin
    validate_simplification ();
    validate_proof_record ();
    validate_other ();
  end

let fptaylor ~input_files =
  if Config.is_option_defined "fpcore-out" then begin
    open_file "fpcore-out" (Config.get_string_option "fpcore-out")
  end;
  if Config.is_option_defined "export-options" then begin
    let out_name = Config.get_string_option "export-options" in
    if out_name <> "" then begin
      Log.report `Important "Exporting options into: %s" out_name;
      open_file "config-out" out_name;
      Config.export_options (get_file_formatter "config-out");
      close_file "config-out"
    end
  end;
  Log.report `Main "";
  let results = input_files
    |> List.fold_left (fun rs fname -> rs @ process_input fname) []
    |> List.map fst in
  close_all ();
  results
