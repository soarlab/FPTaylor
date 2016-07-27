(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT licence           *)
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
open Log


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

let print_problem_info =
  let print_opt str = function
    | None -> ()
    | Some v -> report (Printf.sprintf "%s: %e" str v) in
  fun pi ->
    report "-------------------------------------------------------------------------------";
    report (Printf.sprintf "Problem: %s\n" pi.name);
    report (Printf.sprintf "Bounds (without rounding): [%e, %e]" pi.real_min pi.real_max);
    print_opt "Absolute error (approximate)" pi.abs_error_approx;
    print_opt "Absolute error (exact)" pi.abs_error_exact;
    print_opt "Relative error (approximate)" pi.rel_error_approx;
    print_opt "Relative error (exact)" pi.rel_error_exact;
    report (Printf.sprintf "\nElapsed time: %.5f\n" pi.elapsed_time)
  
let get_problem_error pi =
  let get_val v =
    match v with
      | Some x -> x
      | None -> infinity in
  let e1 = get_val pi.abs_error_approx and
      e2 = get_val pi.abs_error_exact in
  min e1 e2

let simplification_flag = ref (Config.get_bool_option "simplification")

let exprs () = env.expressions

let var_bound_float name = variable_interval name

let print_form f =
  let _ = report (Format.sprintf "v0 = %s" (print_expr_str f.v0)) in
  let _ = map (fun (e, err) -> 
    report (Format.sprintf "%d (%d): exp = %d: %s" 
	      err.index err.proof_index err.exp (print_expr_str e))) f.v1 in
  let _ = report "\nCorresponding original subexpressions:" in
  let _ = map (fun (_, err) ->
    let i = err.index in
    if i > 0 then
      let expr = expr_for_index i in
      report (Format.sprintf "%d: %s" i (print_expr_str expr))
    else ()) f.v1 in
  ()


let add2_symbolic (e1, exp1) (e2, exp2) =
  (* Swap if exp1 > exp2 *)
  let e1, exp1, e2, exp2 = if exp1 <= exp2 then e1, exp1, e2, exp2 else e2, exp2, e1, exp1 in
  if exp1 = 0 then 
    (if exp2 = 0 then (fp_to_const 0.0, 0) else (e2, exp2))
  else if exp2 = 0 then 
    (e1, exp1)
  else if exp1 = exp2 then
    (mk_add e1 e2, exp1)
  else
    let eps = get_eps (exp1 - exp2) in
    (mk_add (mk_mul (fp_to_const eps) e1) e2, exp2)


let sum_symbolic s = itlist add2_symbolic s (const_0, 0)


let errors =
  let compute_bound (e, err) =
    let bound = Opt.optimize_abs Opt_common.default_opt_pars e in
    let _ = report (Format.sprintf "%d: exp = %d: %f" err.index err.exp bound) in
    bound, err.exp 
  in
  let rec split es =
    match es with
      | [] -> [], []
      | (e, err) :: t ->
	let es1, es2 = split t in
	if err.index < 0 then
	  es1, (e, err) :: es2
	else
	  (e, err) :: es1, es2
  in
  let abs_error f =
    let v1, v2 = split f.v1 in
    let bounds2' = map compute_bound v2 in
    let bounds2 = map (fun (e, exp) -> make_stronger e, exp) bounds2' in
    let total2', exp2 = sum_high bounds2 in
    let total2 = get_eps exp2 *^ total2' in
    let err_approx =
      if Config.get_bool_option "opt-approx" then
	let _ = report "\nSolving the approximate optimization problem" in
	let _ = report "\nAbsolute errors:" in
	let bounds1' = map compute_bound v1 in
	let bounds1 = map (fun (e, exp) -> make_stronger e, exp) bounds1' in
	let total1', exp1 = sum_high bounds1 in
	let total1 = get_eps exp1 *^ total1' in
	let total = make_stronger (total1 +^ total2) in
	let all_bounds = map fst bounds1 @ map fst bounds2 in
	let all_indices = map (fun (_, err) -> err.proof_index) v1 
	  @ map (fun (_, err) -> err.proof_index) v2 in
	let _ = Proof.add_opt_approx all_indices all_bounds total in
	let _ = report (Format.sprintf "total1: %e\ntotal2: %e\ntotal: %e" total1 total2 total) in
	Some total
      else None in
    let err_exact =
      if Config.get_bool_option "opt-exact" then
	let _ = report "\nSolving the exact optimization problem" in
	let abs_exprs = map (fun (e, err) -> mk_abs e, err.exp) v1 in
	let full_expr', exp = sum_symbolic abs_exprs in
	let full_expr = if !simplification_flag then Maxima.simplify full_expr' else full_expr' in

	let _ = 
	  Out_racket.create_racket_file "abs_exact" 
	    "fptaylor-abs" total2 exp full_expr;
	  Out_test.create_test_file "test_abs_exact.txt" full_expr in

	let bound = Opt.optimize_abs Opt_common.default_opt_pars full_expr in
	let total = 
	  if Config.proof_flag then
	    let e' = get_eps exp in
	    let e = if e' = 0.0 then 1.0 else e' in
	    let bound = make_stronger (bound +^ Fpu.fdiv_high total2 e) in
	    let total = e *^ bound in
	    let _ = Proof.add_opt_exact bound exp total in
	    total
	  else
	    (get_eps exp *^ bound) +^ total2 in
	let _ = report (Format.sprintf "exact bound (exp = %d): %f" exp bound) in
	let _ = report (Format.sprintf "exact total: %e\ntotal2: %e" total total2) in
	Some total
      else None in
    err_approx, err_exact
  in
  let rel_error f (f_min, f_max) =
    let f_int = {low = f_min; high = f_max} in
    let rel_tol = 0.0001 in
    if (abs_I f_int).low < rel_tol then
      let _ = report "\nCannot compute the relative error: values of the function are close to zero" in
      None, None
    else
      let v1, v2 = split f.v1 in
      let v1 = map (fun (e, err) -> mk_div e f.v0, err) v1 in
      let v1 = 
	if !simplification_flag then map (fun (e, err) -> Maxima.simplify e, err) v1 else v1 in
      let bounds2 = map compute_bound v2 in
      let total2', exp2 = sum_high bounds2 in
      let total2 = get_eps exp2 *^ total2' in
      let b2 = (total2 /.$ abs_I f_int).high in
      let err_approx =
	if Config.get_bool_option "opt-approx" then
	  let _ = report "\nSolving the approximate optimization probelm" in
	  let _ = report "\nRelative errors:" in
	  let bounds1 = map compute_bound v1 in
	  let total1', exp1 = sum_high bounds1 in
	  let total1 = get_eps exp1 *^ total1' in
	  let total = total1 +^ b2 in
	  let _ = report (Format.sprintf "rel-total1: %e\nrel-total2: %e\nrel-total: %e" total1 b2 total) in
	  Some total
	else None in
      let err_exact =
	if Config.get_bool_option "opt-exact" then
	  let _ = report "\nSolving the exact optimization problem" in
	  let abs_exprs = map (fun (e, err) -> mk_abs e, err.exp) v1 in
	  let full_expr', exp = sum_symbolic abs_exprs in
	  let full_expr = if !simplification_flag then Maxima.simplify full_expr' else full_expr' in

	  let _ = 
	    Out_racket.create_racket_file "rel_exact" 
	      "fptaylor-rel" b2 exp full_expr;
	    Out_test.create_test_file "test_rel_exact.txt" full_expr in

	  let bound = Opt.optimize_abs Opt_common.default_opt_pars full_expr in
	  let _ = report (Format.sprintf "exact bound-rel (exp = %d): %f" exp bound) in
	  let total = (get_eps exp *^ bound) +^ b2 in
	  let _ = report (Format.sprintf "exact total-rel: %e\ntotal2: %e" total b2) in
	  Some total
	else None in
      err_approx, err_exact
  in
  fun pi form ->
    let f_min, f_max = 
      if Config.get_bool_option "rel-error" || (Config.get_bool_option "find-bounds") then
	Opt.optimize Opt_common.default_opt_pars form.v0
      else
	neg_infinity, infinity in
    let _ = report (Format.sprintf "bounds: [%e, %e]" f_min f_max) in
    let pi = {pi with real_min = f_min; real_max = f_max} in
    let pi =
      if Config.get_bool_option "opt-approx" || Config.get_bool_option "opt-exact" then
	let abs_approx, abs_exact = 
	  if Config.get_bool_option "abs-error" then abs_error form else None, None in
	let rel_approx, rel_exact = 
	  if Config.get_bool_option "rel-error" then rel_error form (f_min, f_max) else None, None in
	{pi with 
	  abs_error_approx = abs_approx;
	  abs_error_exact = abs_exact;
	  rel_error_approx = rel_approx;
	  rel_error_exact = rel_exact
	}
      else pi in
    let _ = report "" in
    pi

let safety_check e =
  try
    Rounding_simpl.check_expr var_bound_float e
  with Rounding_simpl.Exceptional_operation (e0, str) ->
    let msg =
      Printf.sprintf "\nPotential exception detected: %s at:\n%s"
	str (print_expr_str e0) in
    if Config.fail_on_exception then
      failwith msg
    else
      let _ = warning msg in
	zero_I

let compute_form pi e =
  let _ = report "\n*************************************" in
  let _ = report ("Taylor form for: " ^ print_expr_str e) in
  let _ = if Config.proof_flag then Proof.new_proof () in
  let start = Unix.gettimeofday() in
  let pi, tform = 
    try
      let bound0 = safety_check e in
      let _ = report ("\nConservative bound: " ^ (sprintf_I "%f" bound0)) in
      let e = Rounding_simpl.simplify_rounding e in
      let _ = report ("\nSimplified rounding: " ^ print_expr_str e) in
      let vars = var_bound_float in
      let _ = Log.report "Building Taylor forms..." in
      let form' = build_form vars e in
      let _ = Log.report "Simplifying Taylor forms..." in
      let form = simplify_form vars form' in
      let _ = Log.report "success" in
      let form = 
	if !simplification_flag then {
	  form_index = form.form_index;
	  v0 = Maxima.simplify form.v0;
	  v1 = map (fun (e, err) -> (if err.index < 0 then e else Maxima.simplify e), err) form.v1;
	}
	else
	  form in
      let _ = print_form form in
      let _ = report "" in
      let pi = errors pi form in
      pi, form
    with Failure msg -> let _ = error msg in pi, dummy_tform
  in
  let stop = Unix.gettimeofday() in
  let _ = report (Format.sprintf "Elapsed time: %.5f" (stop -. start)) in
  let _ = 
    if Config.proof_flag then
      let _ = report ("Saving a proof certificate for " ^ pi.name) in
      Proof.save_proof (pi.name ^ ".proof") in
  {pi with elapsed_time = stop -. start}, tform

let approximate_constraint pi c =
  let e = 
    match c with
      | Le (a, b) -> mk_sub a b
      | Lt (a, b) -> mk_sub a b
      | Eq (a, b) -> failwith "approximate_constraint: Eq is not supported" in
  let _ = report "Constraint form" in
  let r, tform = compute_form pi e in
  let err = get_problem_error r in
  let _ = report (Printf.sprintf "\n%s error: %e\n" r.name err) in
  Le (tform.v0, Const (const_of_float err))


let process_input fname =
  let _ = report ("Loading: " ^ fname) in
  let _ = open_log (Filename.concat "log" fname) in
  let _ =
    match log_fmt() with
      | Some fmt -> Config.print_options fmt
      | _ -> () in
  let _ = parse_file fname in
  let names, es = unzip (exprs ()) in
  let cnames, cs = unzip (all_constraints ()) in
  let problems0 = map (fun name -> {default_problem_info with name = name}) names in
  let constraints0 = map (fun name -> {default_problem_info with name = name}) cnames in
  let constraints = 
    if cs = [] then [] else
      let _ = report "\n****** Approximating constraints *******\n" in
      map2 approximate_constraint constraints0 cs in
  let _ = set_active_constraints (zip cnames constraints) in
  let problems = map2 compute_form problems0 es in
  let _ = report "*************************************\n" in
  let _ = map (fun (p, _) -> print_problem_info p) problems in
  let _ = close_log () in
  report ""


let main () =
  let p = print_string in
  if Config.input_files = [] then
    let _ = p ("Usage: "
	       ^ Sys.argv.(0) 
	       ^ " [-c config1] [-c config2 ...] input_file1 [input_file2 ...]\n") in
    exit 1
  else
    let _ = Config.print_options Format.std_formatter in
    let _ = 
      if Config.get_bool_option "simplification" && not (Maxima.test_maxima()) then
	begin
 	  Log.error "A computer algebra system Maxima is not installed.";
          Log.error "Simplifications are disabled.";
          Log.error "Go to http://maxima.sourceforge.net/ to install Maxima.";
          simplification_flag := false 
	end
    in
    let _ = map process_input Config.input_files in
    exit 0

let () = main ()
