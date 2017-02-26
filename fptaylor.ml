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
open Lib
open List
open Parser
open Rounding
open Expr
open Environment
open Taylor_form

type problem_info = {
  name : string;
  real_min : float;
  real_max : float;
  abs_error_approx : float option;
  abs_error_exact : float option;
  rel_error_approx : float option;
  rel_error_exact : float option;
  elapsed_time : float;
}

let default_problem_info = {
  name = "NONE";
  real_min = neg_infinity;
  real_max = infinity;
  abs_error_approx = None;
  abs_error_exact = None;
  rel_error_approx = None;
  rel_error_exact = None;
  elapsed_time = 0.0;
}

let get_problem_absolute_error pi =
  let e1 = option_default ~default:infinity pi.abs_error_approx and
      e2 = option_default ~default:infinity pi.abs_error_exact in
  min e1 e2

let print_problem_info =
  let print_opt str = function
    | None -> ()
    | Some v -> Log.report 0 "%s: %e" str v
  in
  let print_bounds pi =
    if pi.real_min > neg_infinity && pi.real_max < infinity then
      begin
        let err = get_problem_absolute_error pi in
        assert (err >= 0.);
        if err < infinity then
          let min = Fpu.fsub_low pi.real_min err and
              max = Fpu.fadd_high pi.real_max err in
          Log.report 0 "Bounds (floating-point): [%.20e, %.20e]" min max
      end
  in
  fun pi ->
    Log.report 0 "-------------------------------------------------------------------------------";
    Log.report 0 "Problem: %s\n" pi.name;
    Log.report 0 "Bounds (without rounding): [%e, %e]" pi.real_min pi.real_max;
    print_bounds pi;
    print_opt "Absolute error (approximate)" pi.abs_error_approx;
    print_opt "Absolute error (exact)" pi.abs_error_exact;
    print_opt "Relative error (approximate)" pi.rel_error_approx;
    print_opt "Relative error (exact)" pi.rel_error_exact;
    Log.report 0 "\nElapsed time: %.2f\n" pi.elapsed_time
  
let exprs () = env.expressions

let var_bound_float name = variable_interval name

let print_form level f =
  Log.report level "v0 = %s" (print_expr_str f.v0);
  iter (fun (e, err) -> 
      Log.report level "%d (%d): exp = %d: %s" 
	         err.index err.proof_index err.exp (print_expr_str e)) f.v1;
  Log.report level "\nCorresponding original subexpressions:";
  iter (fun (_, err) ->
      let i = err.index in
      if i > 0 then
        let expr = expr_for_index i in
        Log.report level "%d: %s" i (print_expr_str expr)) f.v1

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
    let eps = get_eps (exp1 - exp2) in
    (mk_add (mk_mul (mk_float_const eps) e1) e2, exp2)

let sum_symbolic s = itlist add2_symbolic s (const_0, 0)

let compute_bound (expr, err) =
  let bound = Opt.find_max_abs Opt_common.default_opt_pars expr in
  Log.report 2 "%d: exp = %d: %f" err.index err.exp bound;
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
                            
let absolute_errors tf =
  Log.report 1 "\nComputing absolute errors";
  let v1, v2 = split_error_terms tf.v1 in
  let bounds2' = map compute_bound v2 in
  let bounds2 = map (fun (e, exp) -> make_stronger e, exp) bounds2' in
  let total2', exp2 = sum_high bounds2 in
  let total2 = get_eps exp2 *^ total2' in
  let err_approx =
    if not (Config.get_bool_option "opt-approx") then None
    else
      begin
	Log.report 1 "\nSolving the approximate optimization problem";
	Log.report 1 "\nAbsolute errors:";
	let bounds1' = map compute_bound v1 in
	let bounds1 = map (fun (e, exp) -> make_stronger e, exp) bounds1' in
	let total1', exp1 = sum_high bounds1 in
	let total1 = get_eps exp1 *^ total1' in
	let total = make_stronger (total1 +^ total2) in
	let all_bounds = map fst bounds1 @ map fst bounds2 in
	let all_indices = map (fun (_, err) -> err.proof_index) v1 
	                  @ map (fun (_, err) -> err.proof_index) v2 in
	Proof.add_opt_approx all_indices all_bounds total;
	Log.report 1 "total1: %e\ntotal2: %e\ntotal: %e" total1 total2 total;
	Some total
      end
  in
  let err_exact =
    if not (Config.get_bool_option "opt-exact") then None
    else
      begin
	Log.report 1 "\nSolving the exact optimization problem";
	let abs_exprs = map (fun (e, err) -> mk_abs e, err.exp) v1 in
	let full_expr', exp = sum_symbolic abs_exprs in
	let full_expr = if Config.get_bool_option "maxima-simplification" then
                          Maxima.simplify full_expr'
                        else
                          full_expr' in

	let _ = 
	  Out_racket.create_racket_file "abs_exact" 
	                                "fptaylor-abs" total2 exp full_expr;
	  Out_test.create_test_file "test_abs_exact.txt" full_expr in

	let bound = Opt.find_max Opt_common.default_opt_pars full_expr in
	let total = 
	  if Config.proof_flag then begin
	      let e' = get_eps exp in
	      let e = if e' = 0.0 then 1.0 else e' in
	      let bound = make_stronger (bound +^ Fpu.fdiv_high total2 e) in
	      let total = e *^ bound in
	      Proof.add_opt_exact bound exp total;
	      total
            end
	  else
	    (get_eps exp *^ bound) +^ total2 in
	Log.report 1 "exact bound (exp = %d): %f" exp bound;
	Log.report 1 "exact total: %e\ntotal2: %e" total total2;
	Some total
      end
  in
  err_approx, err_exact

let relative_errors tf (f_min, f_max) =
  Log.report 1 "\nComputing relative errors";
  let f_int = {low = f_min; high = f_max} in
  let rel_tol = 0.0001 in
  if (abs_I f_int).low < rel_tol then begin
      Log.warning 0 "\nCannot compute the relative error: values of the function are close to zero";
      None, None
    end
  else
    let v1, v2 = split_error_terms tf.v1 in
    let v1 = map (fun (e, err) -> mk_div e tf.v0, err) v1 in
    let v1 = 
      if Config.get_bool_option "maxima-simplification" then
        map (fun (e, err) -> Maxima.simplify e, err) v1
      else
        v1 in
    let bounds2 = map compute_bound v2 in
    let total2', exp2 = sum_high bounds2 in
    let total2 = get_eps exp2 *^ total2' in
    let b2 = (total2 /.$ abs_I f_int).high in
    let err_approx =
      if not (Config.get_bool_option "opt-approx") then None
      else
        begin
	  Log.report 1 "\nSolving the approximate optimization probelm";
	  Log.report 1 "\nRelative errors:";
	  let bounds1 = map compute_bound v1 in
	  let total1', exp1 = sum_high bounds1 in
	  let total1 = get_eps exp1 *^ total1' in
	  let total = total1 +^ b2 in
	  Log.report 1 "rel-total1: %e\nrel-total2: %e\nrel-total: %e" total1 b2 total;
	  Some total
        end
    in
    let err_exact =
      if not (Config.get_bool_option "opt-exact") then None
      else
        begin
	  Log.report 1 "\nSolving the exact optimization problem";
	  let abs_exprs = map (fun (e, err) -> mk_abs e, err.exp) v1 in
	  let full_expr', exp = sum_symbolic abs_exprs in
	  let full_expr = if Config.get_bool_option "maxima-simplification" then
                            Maxima.simplify full_expr'
                          else
                            full_expr' in
	  let _ = 
	    Out_racket.create_racket_file "rel_exact" 
	                                  "fptaylor-rel" b2 exp full_expr;
	    Out_test.create_test_file "test_rel_exact.txt" full_expr in
          
	  let bound = Opt.find_max Opt_common.default_opt_pars full_expr in
	  Log.report 1 "exact bound-rel (exp = %d): %f" exp bound;
	  let total = (get_eps exp *^ bound) +^ b2 in
	  Log.report 1 "exact total-rel: %e\ntotal2: %e" total b2;
	  Some total
        end
    in
    err_approx, err_exact

let errors pi tform =
  let f_min, f_max = 
    if Config.get_bool_option "rel-error" || Config.get_bool_option "find-bounds" then
      Opt.find_min_max Opt_common.default_opt_pars tform.v0
    else
      neg_infinity, infinity in
  Log.report 1 "bounds: [%e, %e]" f_min f_max;
  let pi = {pi with real_min = f_min; real_max = f_max} in
  let pi =
    if Config.get_bool_option "opt-approx" || Config.get_bool_option "opt-exact" then
      let abs_approx, abs_exact = 
	if Config.get_bool_option "abs-error" then
          absolute_errors tform
        else
          None, None in
      let rel_approx, rel_exact = 
	if Config.get_bool_option "rel-error" then
          relative_errors tform (f_min, f_max)
        else
          None, None in
      {pi with
	abs_error_approx = abs_approx;
	abs_error_exact = abs_exact;
	rel_error_approx = rel_approx;
	rel_error_exact = rel_exact
      }
    else
      pi in
  Log.report 1 "";
  pi

let safety_check e =
  try
    Rounding_simpl.check_expr var_bound_float e
  with Rounding_simpl.Exceptional_operation (e0, str) ->
    let msg =
      Format.sprintf "\nPotential exception detected: %s at:\n%s"
	             str (print_expr_str e0) in
    if Config.fail_on_exception then
      failwith msg
    else
      (Log.warning_str 0 msg; zero_I)

let compute_form pi e =
  Log.report 2 "\n*************************************";
  Log.report 2 "Taylor form for: %s" (print_expr_str e);
  if Config.proof_flag then Proof.new_proof ();
  let start = Unix.gettimeofday() in
  let pi, tform = 
    try
      let bound0 = safety_check e in
      Log.report 2 "\nConservative bound: %s" (sprintf_I "%f" bound0);
      let e = Rounding_simpl.simplify_rounding e in
      Log.report 2 "\nSimplified rounding: %s" (print_expr_str e);
      let vars = var_bound_float in
      Log.report 1 "Building Taylor forms...";
      let form' = build_form vars e in
      Log.report 1 "Simplifying Taylor forms...";
      let form = simplify_form vars form' in
      Log.report 1 "success";
      let form = 
	if Config.get_bool_option "maxima-simplification" then {
	  form_index = form.form_index;
	  v0 = Maxima.simplify form.v0;
	  v1 = map (fun (e, err) -> (if err.index < 0 then e else Maxima.simplify e), err) form.v1;
	}
	else
	  form in
      print_form 2 form;
      Log.report 2 "";
      let pi = errors pi form in
      pi, form
    with Failure msg ->
      Log.error_str msg;
      pi, dummy_tform
  in
  let stop = Unix.gettimeofday() in
  Log.report 2 "Elapsed time: %.5f" (stop -. start);
  let () = 
    if Config.proof_flag then
      begin
        let proof_dir = Config.get_string_option "proof-dir" in
        Log.report 1 "Saving a proof certificate for %s (in %s)" pi.name proof_dir;
        Proof.save_proof proof_dir (pi.name ^ ".proof")
      end
  in
  {pi with elapsed_time = stop -. start}, tform

let approximate_constraint pi c =
  let e = 
    match c with
      | Le (a, b) -> mk_sub a b
      | Lt (a, b) -> mk_sub a b
      | Eq (a, b) -> failwith "approximate_constraint: Eq is not supported" in
  Log.report 1 "Constraint form";
  let r, tform = compute_form pi e in
  let err = get_problem_absolute_error r in
  Log.report 1 "\n%s error: %e\n" r.name err;
  Le (tform.v0, mk_float_const err)

let process_input fname =
  Log.report 0 "Loading: %s" fname;
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
  Config.print_options 4;
  let _ = parse_file fname in
  let names, es = unzip (exprs ()) in
  let cnames, cs = unzip (all_constraints ()) in
  let problems0 = map (fun name -> {default_problem_info with name = name}) names in
  let constraints0 = map (fun name -> {default_problem_info with name = name}) cnames in
  let constraints = 
    if cs = [] then [] else begin
        Log.report 1 "\n****** Approximating constraints *******\n";
        map2 approximate_constraint constraints0 cs
      end in
  let _ = set_active_constraints (zip cnames constraints) in
  let problems = map2 compute_form problems0 es in
  Log.report 2 "*************************************\n";
  iter (fun (p, _) -> print_problem_info p) problems;
  Log.close ();
  Log.report 0 ""

let validate_options () =
  let validate_simplification () =
    if Config.get_bool_option "maxima-simplification" && not (Maxima.test_maxima()) then
      begin
        Log.warning 0 "A computer algebra system Maxima is not installed. \
                       Simplifications are disabled. \
                       Go to http://maxima.sourceforge.net/ to install Maxima.";
        Config.add_option "maxima-simplification" "false"
      end
  in
  let validate_proof_record () =
    if Config.get_bool_option "proof-record" then
      if Config.get_bool_option "fp-power2-model" then
        begin
          Log.warning 0 "Proof certificates (proof-record = true) are not implemented for \
                         the improved rounding model (fp-power2-model = true).";
        end
      else if Config.get_bool_option "develop" then
        begin
          Log.warning 0 "Proof certificates (proof-record = true) are not implemented for \
                         some features of the development mode (develop = true).";
        end
  in
  begin
    validate_simplification ();
    validate_proof_record ();
  end
         
let main () =
  Log.report 0 "FPTaylor, version %s" Version.version;
  if Config.input_files = [] then
    begin
      Printf.printf
        "\nUsage: %s [--opt-name opt-value ...] [-c config1 ...] \
         input_file1 [input_file2 ...]\n\n\
         See default.cfg for a list of available options.\n\n"
        Sys.argv.(0);
      exit 1
    end
  else
    begin
      validate_options ();
      Log.report 0 "";
      iter process_input Config.input_files;
      exit 0
    end

let () = main ()
