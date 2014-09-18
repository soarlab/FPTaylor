(* FPTaylor                                                                   *)
(* Alexey Solovyev, University of Utah                                        *)

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
  real_min = 0.0;
  real_max = 0.0;
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
  


let exprs () = env.expressions

let var_bound_float name = variable_interval name

let var_bound_rat name =
  let v = find_variable name in
  v.lo_bound.rational_v, v.hi_bound.rational_v


let opt tol e =
  let min, max =
    match Config.opt with
      | "z3" -> Opt_z3.min_max_expr tol var_bound_rat e
      | "bb" -> Opt_basic_bb.min_max_expr tol tol var_bound_float e
      | "nlopt" -> Opt_nlopt.min_max_expr tol var_bound_float e
      | s -> failwith ("Unsupported optimization engine: " ^ s) in
  min, max


let print_form f =
  let _ = report (Format.sprintf "v0 = %s" (print_expr_str f.v0)) in
  let _ = map (fun (e, err) -> 
    report (Format.sprintf "%d: exp = %d: %s" err.index err.exp (print_expr_str e))) f.v1 in
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
  let abs (a, b) =
    let x = abs_float a and
	y = abs_float b in
    max x y 
  in
  let compute_bound tol (e, err) =
    let min, max = opt tol e in
    let bound = abs (min, max) in
    let _ = report (Format.sprintf "%d: exp = %d: %f, %f (%f)" err.index err.exp min max bound) in
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
  let abs_error tol f =
    let v1, v2 = split f.v1 in
    let bounds2 = map (compute_bound tol) v2 in
    let total2', exp2 = sum_high bounds2 in
    let total2 = get_eps exp2 *^ total2' in
    let err_approx =
      if Config.opt_approx then
	let _ = report "\nSolving the approximate optimization problem" in
	let _ = report "\nAbsolute errors:" in
	let bounds1 = map (compute_bound tol) v1 in
	let total1', exp1 = sum_high bounds1 in
	let total1 = get_eps exp1 *^ total1' in
	let total = total1 +^ total2 in
	let _ = report (Format.sprintf "total1: %e\ntotal2: %e\ntotal: %e" total1 total2 total) in
	Some total
      else None in
    let err_exact =
      if Config.opt_exact then
	let _ = report "\nSolving the exact optimization problem" in
	let abs_exprs = map (fun (e, err) -> mk_abs e, err.exp) v1 in
	let full_expr', exp = sum_symbolic abs_exprs in
	let full_expr = if Config.simplification then Maxima.simplify full_expr' else full_expr' in
	let min, max = opt tol full_expr in
	let _ = report (Format.sprintf "exact min, max (exp = %d): %f, %f" exp min max) in
	let total = (get_eps exp *^ abs (min, max)) +^ total2 in
	let _ = report (Format.sprintf "exact total: %e" total) in
	Some total
      else None in
    err_approx, err_exact
  in
  let rel_error tol f (f_min, f_max) =
    let f_int = {low = f_min; high = f_max} in
    let rel_tol = 0.0001 in
    if (abs_I f_int).low < rel_tol then
      let _ = report "\nCannot compute the relative error: values of the function are close to zero" in
      None, None
    else
      let v1, v2 = split f.v1 in
      let v1 = map (fun (e, err) -> mk_div e f.v0, err) v1 in
      let v1 = 
	if Config.simplification then map (fun (e, err) -> Maxima.simplify e, err) v1 else v1 in
      let bounds2 = map (compute_bound tol) v2 in
      let total2', exp2 = sum_high bounds2 in
      let total2 = get_eps exp2 *^ total2' in
      let b2 = (total2 /.$ abs_I f_int).high in
      let err_approx =
	if Config.opt_approx then
	  let _ = report "\nSolving the approximate optimization probelm" in
	  let _ = report "\nRelative errors:" in
	  let bounds1 = map (compute_bound tol) v1 in
	  let total1', exp1 = sum_high bounds1 in
	  let total1 = get_eps exp1 *^ total1' in
	  let total = total1 +^ b2 in
	  let _ = report (Format.sprintf "rel-total1: %e\nrel-total2: %e\nrel-total: %e" total1 b2 total) in
	  Some total
	else None in
      let err_exact =
	if Config.opt_exact then
	  let _ = report "\nSolving the exact optimization problem" in
	  let abs_exprs = map (fun (e, err) -> mk_abs e, err.exp) v1 in
	  let full_expr', exp = sum_symbolic abs_exprs in
	  let full_expr = if Config.simplification then Maxima.simplify full_expr' else full_expr' in
	  let min, max = opt tol full_expr in
	  let _ = report (Format.sprintf "exact min-rel, max-rel (exp = %d): %f, %f" exp min max) in
	  let total = (get_eps exp *^ abs (min, max)) +^ b2 in
	  let _ = report (Format.sprintf "exact total-rel: %e" total) in
	  Some total
	else None in
      err_approx, err_exact
  in
  fun pi form ->
    let tol = Config.opt_tol in
    let f_min, f_max = opt tol form.v0 in
    let _ = report (Format.sprintf "bounds: [%e, %e]" f_min f_max) in
    let pi = {pi with real_min = f_min; real_max = f_max} in
    let pi =
      if Config.opt_approx || Config.opt_exact then
	let abs_approx, abs_exact = 
	  if Config.abs_error then abs_error tol form else None, None in
	let rel_approx, rel_exact = 
	  if Config.rel_error then rel_error tol form (f_min, f_max) else None, None in
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
  let start = Unix.gettimeofday() in
  let pi = 
    try
      let bound0 = safety_check e in
      let _ = report ("\nConservative bound: " ^ (sprintf_I "%f" bound0)) in
      let e = Rounding_simpl.simplify_rounding e in
      let _ = report ("\nSimplified rounding: " ^ print_expr_str e) in
      let vars = var_bound_float in
      let form' = build_form vars e in
      let form = simplify_form vars form' in
      let form = 
	if Config.simplification then {
	  v0 = Maxima.simplify form.v0;
	  v1 = map (fun (e, err) -> (if err.index < 0 then e else Maxima.simplify e), err) form.v1;
	}
	else
	  form in
      let _ = print_form form in
      let _ = report "" in
      let pi = errors pi form in
      pi
    with Failure msg -> let _ = error msg in pi
  in
  let stop = Unix.gettimeofday() in
  let _ = report (Format.sprintf "Elapsed time: %.5f" (stop -. start)) in
  {pi with elapsed_time = stop -. start}


let process_input fname =
  let _ = report ("Loading: " ^ fname) in
  let _ = open_log (Filename.concat "log" fname) in
  let _ =
    match log_fmt() with
      | Some fmt -> Config.print_options fmt
      | _ -> () in
  let _ = parse_file fname in
  let names, es = unzip (exprs ()) in
  let problems0 = map (fun name -> {default_problem_info with name = name}) names in
  let problems = map2 compute_form problems0 es in
  let _ = report "*************************************\n" in
  let _ = map print_problem_info problems in
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
    let _ = map process_input Config.input_files in
    exit 0

let _ = main ()
